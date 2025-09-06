package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server.Log
import org.updraft0.controltower.server.auth.{EsiError, Users, asThrowable}
import org.updraft0.esi.client.{EsiClient, ServerStatusResponse}
import zio.*
import zio.stream.ZStream

object CharacterAffiliationTracker:
  type Env = ServerStatusTracker & EsiClient & Users.Env

  // Every poll interval, update character affiliations
  private val PollInterval = 10.minutes

  // Limit how many characters to put in a batch
  private val EsiMaxCharacterPerBatch = 64

  private val RefreshSchedule = Schedule.exponential(2.seconds) && Schedule.recurs(3)

  def layer: ZLayer[Env, Nothing, Unit] =
    ZLayer.scoped(apply())

  def apply(): ZIO[Scope & Env, Nothing, Unit] =
    val refresh = refreshAll
      .logError("Failed to refresh all character affiliations")
      .ignore @@ Log.BackgroundOperation("affiliationTracker")

    refresh.delay(5.seconds).forkScoped *> refresh.repeat(Schedule.fixed(PollInterval)).forkScoped.unit

  private def refreshAll: ZIO[Env, Throwable, Unit] =
    ZIO
      .serviceWithZIO[ServerStatusTracker](_.status)
      .flatMap:
        case Left(_) =>
          ZIO.logWarning("Not refreshing due to errored service status")
        case Right(s) if !s.isOnlineEnough =>
          ZIO.logWarning("Not refreshing due to server not having enough players online or being VIP")
        case Right(_) =>
          Users.allCharacters
            .flatMap(allChars =>
              ZStream
                .fromChunk(allChars)
                .grouped(EsiMaxCharacterPerBatch)
                .mapZIO(getAndSaveAffiliations)
                .runDrain
            )
            .mapError(esiToThrowable)
            .retry(RefreshSchedule)

  private def getAndSaveAffiliations(charIds: Chunk[CharacterId]) =
    ZIO
      .serviceWithZIO[EsiClient](_.getCharacterAffiliations(charIds.toList).mapError(EsiError.ClientError.apply))
      .flatMap(xs =>
        Users
          .updateAffiliations(xs.map(ca => (ca.characterId, ca.corporationId, ca.allianceId)))
      )
      .logError(s"Failed to get character affiliations for $charIds")

  private def esiToThrowable(e: Users.Error): Throwable =
    e match
      case sqlx: java.sql.SQLException => sqlx
      case esi: EsiError               => esi.asThrowable

extension (v: ServerStatusResponse) def isOnlineEnough: Boolean = v.players > 100 && v.vip.forall(!_)
