package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server.auth.{CharacterAuth, Users}
import org.updraft0.controltower.server.Log
import org.updraft0.esi.client.EsiClient
import zio.*

trait CharacterAuthTracker:
  def newLogin(auth: CharacterAuth): UIO[Unit]
  def updates: URIO[Scope, Dequeue[Chunk[CharacterAuth]]]

object CharacterAuthTracker:
  // Every poll interval, refresh the tokens that need to be refreshed
  private val PollInterval = 15.seconds

  // Every snapshot interval, send out the current universe of auth tokens
  private val SnapshotInterval = 10.minutes

  // Limit number of in flight messages (hub is dropping anyway so should not matter too much)
  private val HubCapacity = 32

  // Limit how many parallel token refreshes to do
  private val EsiParallel = 32

  def layer: ZLayer[Users.Env, Throwable, CharacterAuthTracker] = ZLayer.scoped(apply())

  def apply() =
    for
      esi   <- ZIO.service[EsiClient]
      state <- Ref.make(Map.empty[CharacterId, Option[CharacterAuth]])
      hub   <- Hub.dropping[Chunk[CharacterAuth]](HubCapacity)
      // load the state of auth from the database
      all <- Users.loadAll.tap(us => ZIO.logDebug(s"Loaded ${us.size} characters"))
      _   <- state.update(_ => all.map(ca => ca.characterId -> Some(ca)).toMap)
      // start the refresh timer
      _ <- refreshPending(esi, state, hub).repeat(Schedule.fixed(PollInterval)).forkScoped
      // start the snapshot timer
      _ <- sendSnapshot(state, hub)
        .repeat(Schedule.duration(1.seconds).andThen(Schedule.fixed(SnapshotInterval)))
        .forkScoped
    yield new CharacterAuthTracker:
      override def newLogin(auth: CharacterAuth): UIO[Unit] = ???
      override def updates: URIO[Scope, Dequeue[Chunk[CharacterAuth]]] =
        hub.subscribe

  private def refreshPending(
      esi: EsiClient,
      state: Ref[Map[CharacterId, Option[CharacterAuth]]],
      q: Enqueue[Chunk[CharacterAuth]]
  ) =
    for
      now <- ZIO.clockWith(_.instant)
      nowExp = now.plus(PollInterval)
      curr <- state.get
      staleEntries = curr.view.values.filter(_.exists(_.expiry.isBefore(nowExp))).map(_.get)
      refreshedAll <- ZIO.foreachExec(Chunk.fromIterable(staleEntries))(ExecutionStrategy.ParallelN(EsiParallel))(ca =>
        Users
          .refreshToken(ca.refreshToken)
          .logError("Failed to refresh token for character")
          .fold(
            _ => None,
            ca => Some(ca)
          ) @@ Log.CharacterId(ca.characterId) @@ Log.BackgroundOperation("authTracker")
      )
      refreshed = refreshedAll.flatten
      _ <- ZIO.logDebug(s"Refreshed token for ${refreshed.size} characters").when(refreshed.nonEmpty)
      _ <- state
        .update(m => refreshed.foldLeft(m)((s, ca) => s.updated(ca.characterId, Some(ca))))
        .when(refreshed.nonEmpty)
      _ <- q.offer(refreshed).when(refreshed.nonEmpty)
    yield ()

  private def sendSnapshot(state: Ref[Map[CharacterId, Option[CharacterAuth]]], q: Enqueue[Chunk[CharacterAuth]]) =
    for
      now <- ZIO.clockWith(_.instant)
      nowExp = now.plus(PollInterval)
      curr <- state.get
      nonStaleEntries = curr.view.filter(_._2.exists(_.expiry.isAfter(nowExp))).map(_._2.get)
      _ <- q.offer(Chunk.fromIterable(nonStaleEntries))
    yield ()
