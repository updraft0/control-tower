package org.updraft0.minireactive

import zio.*
import zio.logging.LogAnnotation
import zio.metrics.*

import java.util.concurrent.TimeoutException

// TODO:
//  - [ ] Comprehensive tests

/** A "reactive entity" is like a "persistent actor" - it has a typed {in,out}box, a hydrate/onStart state recovery
  * mechanism and ability to send/subscribe to messages
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
  def hydrate(key: K, in: Enqueue[I]): URIO[Scope & R, S]

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

  def destroy(key: K): UIO[Unit]

  private[minireactive] def restart(key: K): UIO[Unit]

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

  def apply[R, K: Tag, S, I, O](
      entity: ReactiveEntity[R, K, S, I, O],
      config: MiniReactiveConfig
  ): URIO[Scope & R, MiniReactive[K, I, O]] =
    (for
      scope <- ZIO.environment[Scope]
      env   <- ZIO.environment[R]
      refs  <- Ref.Synchronized.make(Map.empty[K, State[K, S, I, O]])
      // there's a queue to restart
      restartQ <- Queue.unbounded[K]
    yield (restartQ -> new MiniReactive[K, I, O]:

      override def enqueue(key: K): UIO[Enqueue[I]] =
        lookupOrCreate(key).map(_.inbox) @@ ReactiveEntity.Tag(entity.tag) @@ ReactiveEntity.Id(key)

      override def subscribe(key: K): URIO[Scope, Dequeue[O]] =
        lookupOrCreate(key).flatMap(_.outbox.subscribe) @@ ReactiveEntity.Tag(entity.tag) @@ ReactiveEntity.Id(key)

      override def destroy(key: K): UIO[Unit] =
        refs
          .updateSomeZIO:
            case m if m.contains(key) =>
              stop(m(key)).as(m - key)
        @@ ReactiveEntity.Tag(entity.tag) @@ ReactiveEntity.Id(key)

      private def lookupOrCreate(key: K): UIO[State[K, S, I, O]] =
        refs
          .updateSomeAndGetZIO:
            case m if !m.contains(key) => create(key).provideEnvironment(scope).map(state => m + (key -> state))
          .map(m => m(key))

      private def create(key: K): URIO[Scope, State[K, S, I, O]] =
        for
          _            <- ZIO.logInfo(s"starting entity")
          inbox        <- Queue.bounded[I](config.mailboxSize)
          outbox       <- Hub.bounded[O](config.mailboxSize)
          initialState <- runHydration(key, inbox)
          labels        = Set(MetricLabel("entity_tag", entity.tag), MetricLabel("key", key.toString))
          inboxCounter  = Metric.counter("entity_inbox_count").fromConst(1).tagged(labels)
          outboxCounter = Metric.counter("entity_outbox_count").tagged(labels)
          processingTime = Metric
            .summary("entity_processing_ms", 2.hours, 100, 0.02d, Chunk(0.1, 0.5, 0.95, 0.99))
            .tagged(labels)
          fiber <- runEntity(key, inbox, outbox, initialState, inboxCounter, outboxCounter, processingTime)
        yield State(inbox, outbox, key, initialState, fiber, inboxCounter, outboxCounter, processingTime)

      private def runHydration(key: K, inbox: Queue[I]) =
        entity
          .hydrate(key, inbox)
          .timeoutFailCause(Cause.die(new TimeoutException("entity failed to hydrate in time")))(
            config.handlerTimeout
          )
          .provideEnvironment(env.add(scope.get))
          .flatMap(Ref.make)

      private[minireactive] def restart(key: K): UIO[Unit] =
        ZIO.logDebug("restarting entity") *> refs.updateZIO: m =>
          for
            prev = m(key)
            initialState <- runHydration(key, prev.inbox)
            fiber <- runEntity(
              key,
              prev.inbox,
              prev.outbox,
              initialState,
              prev.inboxCounter,
              prev.outboxCounter,
              prev.processingTime
            ).provideEnvironment(env.add(scope.get))
            next = prev.copy(current = initialState, fiber = fiber)
          yield m.updated(key, next)

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
            .timeoutFailCause(Cause.die(new TimeoutException("entity failed to process message in time")))(
              config.handlerTimeout
            )
            .timed
          (took, res) = resT
          _ <- processingTime.update(took.toMillis.toDouble)
          (nextState, msgs) = res
          _ <- stateRef.set(nextState)
          _ <- outboxCounter.update(msgs.size)
          _ <- outbox.offerAll(msgs)
        yield ()).forever.forkScoped.onError(c => ZIO.logDebugCause(s"failed for $key", c) *> restartQ.offer(key))

      private def stop(state: State[K, S, I, O]): UIO[Unit] =
        ZIO.logInfo("stopping entity") *>
          state.fiber.interrupt.ignoreLogged *>
          state.inbox.shutdown *>
          state.outbox.shutdown
    )).flatMap((restartQ, res) => restartQ.take.flatMap(res.restart).forever.forkScoped.as(res))
