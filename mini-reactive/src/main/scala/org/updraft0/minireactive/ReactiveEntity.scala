package org.updraft0.minireactive

import zio.*
import zio.logging.LogAnnotation
import zio.metrics.*

import java.util.concurrent.TimeoutException

// TODO:
//  - [ ] Monitoring
//  - [ ] Timeouts for mailbox processing
//  - [ ] Comprehensive tests

/** A "reactive entity" is like a "persistent actor"
  *
  * @tparam R
  *   The 'environment' type - aka what effects processing needs
  * @tparam K
  *   The 'key' type - unique identifier for this entity
  * @tparam S
  *   The 'state' type - aka what this entity stores
  * @tparam I
  *   The 'input' type - aka what sort of 'message' this entity can process
  * @tparam O
  *   The 'output' type - aka what sort of 'message' this entity will output
  */
trait ReactiveEntity[R, K, S, I, O]:

  def tag: String

  /** Initial state for this entity with given id
    */
  def hydrate(key: K): URIO[R, S]

  /** Handle a state change based on an input, producing zero or more outputs and the next state
    */
  def handle(key: K, state: S, in: I): URIO[R, (S, Chunk[O])]

object ReactiveEntity:
  val Tag            = LogAnnotation[String]("EntityTag", (_, r) => r, identity)
  def Id[K: zio.Tag] = LogAnnotation[K]("EntityId", (_, r) => r, _.toString)

case class MiniReactiveConfig(mailboxSize: Int, handlerTimeout: Duration)

/** A 'mini-reactive' is something that manages uniquely identifiable entities, allow to enqueue new messages for them,
  * subscribe to updates, and manage lifetimes (by explicitly destroying entities)
  */
trait MiniReactive[K, I, O]:

  def enqueue(key: K): UIO[Enqueue[I]]

  def subscribe(key: K): URIO[Scope, Dequeue[O]]

  def destroy(key: K): UIO[Boolean]

object MiniReactive:
  private case class State[K, S, I, O](
      inbox: Queue[I],
      outbox: Hub[O],
      key: K,
      current: Ref[S],
      fiber: Fiber[Nothing, Nothing],
      inboxCounter: Metric.Counter[Long],
      outboxCounter: Metric.Counter[Long],
      processingTime: Metric.Summary[Double]
  )

  def layer[R, K: Tag, S, I: Tag, O: Tag](
      entity: ReactiveEntity[R, K, S, I, O],
      config: MiniReactiveConfig
  ): ZLayer[R, Nothing, MiniReactive[K, I, O]] =
    ZLayer.scoped(apply(entity, config))

  def apply[R, K: Tag, S, I: Tag, O: Tag](
      entity: ReactiveEntity[R, K, S, I, O],
      config: MiniReactiveConfig
  ): URIO[Scope & R, MiniReactive[K, I, O]] =
    for
      scope <- ZIO.environment[Scope]
      env   <- ZIO.environment[R]
      s     <- Semaphore.make(1)
      refs  <- Ref.make(Map.empty[K, State[K, S, I, O]])
      sup   <- Supervisor.track(true)
    yield new MiniReactive[K, I, O]:

      override def enqueue(key: K): UIO[Enqueue[I]] =
        lookupOrCreate(key).map(_.inbox) @@ ReactiveEntity.Tag(entity.tag) @@ ReactiveEntity.Id[K].apply(key)

      override def subscribe(key: K): URIO[Scope, Dequeue[O]] =
        lookupOrCreate(key).flatMap(_.outbox.subscribe)

      override def destroy(key: K): UIO[Boolean] =
        refs
          .modify(refs => refs.get(key) -> (refs - key))
          .flatMap(ZIO.fromOption)
          .flatMap(stop)
          .as(true)
          .orElseSucceed(false) @@ ReactiveEntity.Tag(entity.tag) @@ ReactiveEntity.Id[K].apply(key)

      private def lookupOrCreate(key: K): UIO[State[K, S, I, O]] =
        s.withPermit(
          refs.get.flatMap(allRefs =>
            allRefs.get(key) match
              case Some(state) => ZIO.succeed(state)
              case None => create(key).provideEnvironment(scope).tap(state => refs.update(m => m + (key -> state)))
          )
        )

      private def create(key: K): URIO[Scope, State[K, S, I, O]] =
        for
          inbox        <- Queue.bounded[I](config.mailboxSize)
          outbox       <- Hub.bounded[O](config.mailboxSize)
          initialState <- entity.hydrate(key).provideEnvironment(env).flatMap(Ref.make) // TODO add timeout here too
          labels        = Set(MetricLabel("entity_tag", entity.tag), MetricLabel("key", key.toString))
          inboxCounter  = Metric.counter("entity_inbox_count").fromConst(1).tagged(labels)
          outboxCounter = Metric.counter("entity_outbox_count").tagged(labels)
          processingTime = Metric
            .summary("entity_processing_ms", 2.hours, 100, 0.02d, Chunk(0.1, 0.5, 0.95, 0.99))
            .tagged(labels)
          fiber <- runEntity(key, inbox, outbox, initialState, inboxCounter, outboxCounter, processingTime)
        yield State(inbox, outbox, key, initialState, fiber, inboxCounter, outboxCounter, processingTime)

      private def runEntity(
          key: K,
          inbox: Queue[I],
          outbox: Hub[O],
          stateRef: Ref[S],
          inboxCounter: Metric.Counter[Long],
          outboxCounter: Metric.Counter[Long],
          processingTime: Metric.Summary[Double]
      ) =
        (for
          inMsg <- inbox.take
          _     <- inboxCounter.update(1)
          state <- stateRef.get
          resT <- entity
            .handle(key, state, inMsg)
            .provideEnvironment(env)
            .timeoutFailCause(Cause.die(new TimeoutException("entity failed to process message")))(
              config.handlerTimeout
            )
            .timed
          (took, res) = resT
          _ <- processingTime.update(took.toMillis.toDouble)
          // FIXME do timeout
          (nextState, msgs) = res
          _ <- stateRef.set(nextState)
          _ <- outboxCounter.update(msgs.size)
          _ <- ZIO.iterate(msgs)(_.nonEmpty)(outbox.offerAll)
        yield ()).forever.supervised(sup).forkScoped

      private def stop(state: State[K, S, I, O]): UIO[Unit] =
        ZIO.logInfo("stopping entity") *>
          state.fiber.interrupt.ignoreLogged *>
          state.inbox.shutdown *>
          state.outbox.shutdown
