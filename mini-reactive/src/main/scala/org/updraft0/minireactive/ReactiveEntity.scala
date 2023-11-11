package org.updraft0.minireactive

import zio.*
import zio.stream.SubscriptionRef

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

  /** Initial state for this entity with given id
    */
  def hydrate(key: K): URIO[R, S]

  /** Handle a state change based on an input, producing zero or more outputs and the next state
    */
  def handle(key: K, state: S, in: I): URIO[R, (S, Chunk[O])]

// Actual manager?

case class MiniReactiveConfig(mailboxSize: Int)

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
      fiber: Fiber[Nothing, Nothing]
  )

  def apply[R, K, S, I, O](
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
        lookupOrCreate(key).map(_.inbox)

      override def subscribe(key: K): URIO[Scope, Dequeue[O]] =
        lookupOrCreate(key).flatMap(_.outbox.subscribe)

      override def destroy(key: K): UIO[Boolean] =
        refs
          .modify(refs => refs.get(key) -> (refs - key))
          .flatMap(ZIO.fromOption)
          .flatMap(stop)
          .as(true)
          .orElseSucceed(false)

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
          initialState <- entity.hydrate(key).provideEnvironment(env).flatMap(Ref.make)
          fiber        <- runEntity(key, inbox, outbox, initialState)
        yield State(inbox, outbox, key, initialState, fiber)

      private def runEntity(key: K, inbox: Queue[I], outbox: Hub[O], stateRef: Ref[S]) =
        (for
          inMsg <- inbox.take
          state <- stateRef.get
          res   <- entity.handle(key, state, inMsg).provideEnvironment(env)
//          _     <- ZIO.logInfo(s"got next state for k ${key} : ${state}")
          (nextState, msgs) = res
          _ <- stateRef.set(nextState)
//          _ <- ZIO.logInfo(s"offering msgs")
          _ <- outbox.offerAll(msgs)
//          _ <- ZIO.logInfo(s"offered ${msgs}")
        yield ()).forever.supervised(sup).forkScoped

      private def stop(state: State[K, S, I, O]): UIO[Unit] =
        state.fiber.interrupt.ignoreLogged

// Example

case class User(id: Long, name: String, age: Int)

enum UserChange:
  case Name(newName: String)
  case Birthday

object UserEntity extends ReactiveEntity[Any, Long, User, UserChange, User]:
  override def hydrate(key: Long) =
    ZIO.succeed(User(key, "Bob", 1)) // everyone is always called Bob

  override def handle(key: Long, state: User, in: UserChange) =
    ZIO.succeed(
      in match
        case UserChange.Name(newName) =>
          val newUser = state.copy(name = newName)
          newUser -> Chunk(newUser)
        case UserChange.Birthday =>
          val newUser = state.copy(age = state.age + 1)
          newUser -> Chunk(newUser)
    )

import zio.stream.ZStream

object MiniReactiveExample extends ZIOAppDefault:
  def run =
    for
      _     <- ZIO.logInfo("Starting")
      r     <- MiniReactive(UserEntity, MiniReactiveConfig(100))
      sub1  <- r.subscribe(1).flatMap(ZStream.fromQueue(_).tap(v => ZIO.logInfo(s"got ${v}")).runDrain).fork
      sub2  <- r.subscribe(2).flatMap(ZStream.fromQueue(_).tap(v => ZIO.logInfo(s"got ${v}")).runDrain).fork
      prod1 <- r.enqueue(1).flatMap(q => q.offer(UserChange.Birthday).delay(5.seconds).repeatN(50)).fork
      prod2 <- r.enqueue(1).flatMap(q => q.offer(UserChange.Name("Alice")).delay(13.seconds)).fork
      _     <- ZIO.logInfo("Started")
      _     <- ZIO.sleep(5.seconds)
      _     <- sub1.dump.flatMap(d => ZIO.logInfo(s"dumped: $d"))
      _     <- ZIO.never
    yield ()
