package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server.auth.Users
import org.updraft0.controltower.server.Log
import org.updraft0.esi.client.EsiClient
import zio.*
import zio.stream.ZStream

object CharacterAffiliationTracker:
  type Env = EsiClient & Users.Env

  // Every poll interval, update character affiliations
  private val PollInterval = 10.minutes

  // Limit how many characters to put in a batch
  private val EsiMaxCharacterPerBatch = 64

  def apply(): ZIO[Scope & Env, Nothing, Unit] =
    (refreshAll
      .tapError(ex => ZIO.logErrorCause("Failed to refresh character affiliations", Cause.fail(ex))) @@
      Log.BackgroundOperation("affiliationTracker")).ignoreLogged
      .repeat(Schedule.duration(5.seconds).andThen(Schedule.fixed(PollInterval)))
      .forkScoped
      .unit

  private def refreshAll: ZIO[Env, Throwable, Unit] =
    Users.allCharacters
      .flatMap(allChars =>
        ZStream
          .fromChunk(allChars)
          .grouped(EsiMaxCharacterPerBatch)
          .mapZIO(getAndSaveAffiliations)
          .runDrain
      )

  private def getAndSaveAffiliations(charIds: Chunk[CharacterId]) =
    ZIO
      .serviceWithZIO[EsiClient](_.getCharacterAffiliations(charIds.toList))
      .tapError(ex => ZIO.logWarning(s"Updating character affiliations for $charIds failed due to $ex"))
      .mapError(_ => new RuntimeException("Updating character affiliations failed due to ESI error"))
      .flatMap(xs => Users.updateAffiliations(xs.map(ca => (ca.characterId, ca.corporationId, ca.allianceId))))
