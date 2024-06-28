package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server.auth.{CharacterAuth, Users}
import org.updraft0.controltower.server.Log
import org.updraft0.esi.client.EsiClient
import zio.*

import java.time.Instant

trait CharacterAuthTracker:
  def newLogin(auth: CharacterAuth): UIO[Unit]
  def logout(characterId: CharacterId): UIO[Unit]
  def updates: URIO[Scope, Dequeue[Chunk[CharacterAuth]]]

object CharacterAuthTracker:
  // Every poll interval, refresh the tokens that need to be refreshed
  private val PollInterval = 20.seconds

  // Multiplier of PollInterval to consider starting to refresh tokens before they expire
  private val PollIntervalExpiryMultiplier = 4

  private val FailedRefreshInterval = 1.minute

  // Every snapshot interval, send out the current universe of auth tokens
  private val SnapshotInterval = 10.minutes

  // Limit number of in flight messages (hub is dropping anyway so should not matter too much)
  private val HubCapacity = 32

  // Limit how many parallel token refreshes to do
  private val EsiParallel = 32

  private enum CharacterState:
    case Active(token: CharacterAuth)
    case RefreshFailure(prev: CharacterAuth, retryCount: Int, nextAt: Instant)

  def layer: ZLayer[Users.Env, Throwable, CharacterAuthTracker] = ZLayer.scoped(apply())

  def apply() =
    for
      esi   <- ZIO.service[EsiClient]
      state <- Ref.make(Map.empty[CharacterId, CharacterState])
      hub   <- Hub.dropping[Chunk[CharacterAuth]](HubCapacity)
      // load the state of auth from the database
      all <- Users.loadAll.tap(us => ZIO.logDebug(s"Loaded ${us.size} characters' auth tokens from DB"))
      _   <- state.update(_ => all.map(ca => ca.characterId -> CharacterState.Active(ca)).toMap)
      // wait for initial refresh
      _ <- refreshPending(esi, state, hub)
      _ <- state.get.flatMap(sm =>
        ZIO.logDebug(s"Have active tokens for ${sm.count {
            case (_, _: CharacterState.Active) => true
            case _                             => false
          }} characters")
      )
      // start the refresh timer
      _ <- refreshPending(esi, state, hub).repeat(Schedule.fixed(PollInterval)).forkScoped
      // start the snapshot timer
      _ <- sendSnapshot(state, hub).delay(10.seconds).forkScoped
      _ <- sendSnapshot(state, hub)
        .repeat(Schedule.fixed(SnapshotInterval))
        .forkScoped
    yield new CharacterAuthTracker:
      override def newLogin(auth: CharacterAuth): UIO[Unit] =
        state.update(_.updated(auth.characterId, CharacterState.Active(auth))) *> sendSnapshot(state, hub)
      override def logout(characterId: CharacterId): UIO[Unit] =
        state.update(_.removed(characterId)) *> sendSnapshot(state, hub)
      override def updates: URIO[Scope, Dequeue[Chunk[CharacterAuth]]] =
        hub.subscribe

  private def refreshPending(
      esi: EsiClient,
      state: Ref[Map[CharacterId, CharacterState]],
      q: Enqueue[Chunk[CharacterAuth]]
  ) =
    for
      now <- ZIO.clockWith(_.instant)
      nowExp = now.plus(PollInterval.multipliedBy(PollIntervalExpiryMultiplier))
      curr <- state.get
      nextStates <- ZIO.foreachExec(curr.keys.zip(curr.values))(ExecutionStrategy.ParallelN(EsiParallel))((cId, cSt) =>
        handleRefresh(cId, cSt, nowExp)
          .map(s => (cId, s)) @@ Log.CharacterId(cId) @@ Log.BackgroundOperation("authTracker")
      )
      opCount = nextStates.map(_._2._1).sum
      _ <- state.set(nextStates.map { case (cId, (_, s)) => (cId, s) }.toMap)
      _ <- q
        .offer(
          Chunk.from(nextStates.collect { case (_, (_, CharacterState.Active(token))) => token })
        )
        .when(opCount > 0)
    yield ()

  private def handleRefresh(characterId: CharacterId, state: CharacterState, nowExp: Instant) =
    state match
      case CharacterState.Active(auth) if auth.expiry.isBefore(nowExp) =>
        // try refreshing an active token
        Users
          .refreshToken(auth.refreshToken)
          .tapError(ex => ZIO.logErrorCause("Failed to refresh token for character", Cause.fail(ex)))
          .fold(
            ex => 0 -> CharacterState.RefreshFailure(auth, 1, nowExp.plus(FailedRefreshInterval)),
            next => 1 -> CharacterState.Active(next)
          )
      case CharacterState.RefreshFailure(prev, count, nextAt) if nextAt.isBefore(nowExp) =>
        // try refreshing a failed token
        Users
          .refreshToken(prev.refreshToken)
          .tapError(ex => ZIO.logErrorCause("Failed to refresh token (again) for character", Cause.fail(ex)))
          .fold(
            ex =>
              0 -> CharacterState
                .RefreshFailure(prev, count + 1, nowExp.plus(FailedRefreshInterval.multipliedBy(count + 1))),
            next => 1 -> CharacterState.Active(next)
          )
      case _ => ZIO.succeed(0 -> state)

  private def sendSnapshot(state: Ref[Map[CharacterId, CharacterState]], q: Enqueue[Chunk[CharacterAuth]]) =
    for
      now <- ZIO.clockWith(_.instant)
      nowExp = now.plus(PollInterval)
      curr <- state.get
      activeEntries = curr.values.collect { case CharacterState.Active(token) =>
        token
      }
      _ <- q.offer(Chunk.from(activeEntries))
    yield ()
