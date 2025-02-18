package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db
import org.updraft0.controltower.db.model
import org.updraft0.controltower.server.{DataCache, Log}
import org.updraft0.esi.client.{
  EsiClient,
  Corporation as EsiCorporation,
  Character as EsiCharacter,
  Alliance as EsiAlliance
}
import zio.*
import zio.query.*

import java.time.Instant

case class GetCorporationById(id: CorporationId) extends Request[Throwable, Option[model.Corporation]] derives CanEqual
case class GetAllianceById(id: AllianceId)       extends Request[Throwable, Option[model.Alliance]] derives CanEqual
case class GetCharacterById(id: CharacterId) extends Request[Throwable, Option[model.IntelCharacter]] derives CanEqual

/** Read-only data service (with expected instant turnaround times)
  */
case class IntelDataSource(
    corporation: DataSource[Any, GetCorporationById],
    alliance: DataSource[Any, GetAllianceById],
    character: DataSource[Any, GetCharacterById]
)

object IntelDataSource:
  type Env = javax.sql.DataSource & EsiClient

  private lazy val GetCorporationFromDbSource = new DataSource.Batched[javax.sql.DataSource, GetCorporationById]:
    override val identifier = "IntelGetCorporationFromDb"
    override def run(requests: Chunk[GetCorporationById])(using Trace) =
      db.query.map
        .getCorporations(requests.map(_.id))
        .foldCause(
          CompletedRequestMap.failCause(requests, _),
          CompletedRequestMap.fromIterableWith(_)(v => GetCorporationById(v.id), v => Exit.succeed(Some(v)))
        )

  private lazy val GetAllianceFromDbSource = new DataSource.Batched[javax.sql.DataSource, GetAllianceById]:
    override val identifier = "IntelGetAllianceFromDb"
    override def run(requests: Chunk[GetAllianceById])(using Trace) =
      db.query.map
        .getAlliances(requests.map(_.id))
        .foldCause(
          CompletedRequestMap.failCause(requests, _),
          CompletedRequestMap.fromIterableWith(_)(v => GetAllianceById(v.id), v => Exit.succeed(Some(v)))
        )

  private lazy val GetCharactersFromDbSource = new DataSource.Batched[javax.sql.DataSource, GetCharacterById]:
    override val identifier = "IntelGetCharacterFromDb"
    override def run(requests: Chunk[GetCharacterById])(using Trace) =
      db.query.map
        .getIntelCharacters(requests.map(_.id))
        .foldCause(
          CompletedRequestMap.failCause(requests, _),
          CompletedRequestMap.fromIterableWith(_)(v => GetCharacterById(v.id), v => Exit.succeed(Some(v)))
        )

  private def getCorporationFromEsiAndSave(
      req: GetCorporationById
  ): URIO[EsiClient & javax.sql.DataSource, Option[model.Corporation]] =
    (for
      esi <- ZIO.service[EsiClient]
      now <- ZIO.clockWith(_.instant)
      esiCorp <- esi
        .getCorporation(req.id)
        .tapError(e => ZIO.logWarning(s"ESI error on getting corporation: $e"))
        .fold(_ => None, Some(_))
      modelCorp = esiCorp.map(c => fromEsiCorporation(req.id, c, now))
      _ <- modelCorp
        .map(c =>
          db.query.map
            .upsertCorporation(c)
            .tapError(ex => ZIO.logError(s"Failed to save corporation in db: $ex"))
            .ignore
        )
        .getOrElse(ZIO.unit)
    yield modelCorp) @@ Log.CorporationId(req.id)

  private def getAllianceFromEsiAndSave(
      req: GetAllianceById
  ): URIO[EsiClient & javax.sql.DataSource, Option[model.Alliance]] =
    (for
      esi <- ZIO.service[EsiClient]
      now <- ZIO.clockWith(_.instant)
      esiAlliance <- esi
        .getAlliance(req.id)
        .tapError(e => ZIO.logWarning(s"ESI error on getting alliance: $e"))
        .fold(_ => None, Some(_))
      modelAlliance = esiAlliance.map(c => fromEsiAlliance(req.id, c, now))
      _ <- modelAlliance
        .map(c =>
          db.query.map
            .upsertAlliance(c)
            .tapError(ex => ZIO.logError(s"Failed to save alliance in db: $ex"))
            .ignore
        )
        .getOrElse(ZIO.unit)
    yield modelAlliance) @@ Log.AllianceId(req.id)

  private def getCharacterFromEsiAndSave(
      req: GetCharacterById
  ): URIO[EsiClient & javax.sql.DataSource, Option[model.IntelCharacter]] =
    (for
      esi <- ZIO.service[EsiClient]
      now <- ZIO.clockWith(_.instant)
      esiChar <- esi
        .getCharacter(req.id)
        .tapError(e => ZIO.logWarning(s"ESI error on getting character: $e"))
        .fold(_ => None, Some(_))
      modelIntelChar = esiChar.map(c => fromEsiCharacter(req.id, c, now))
      _ <- modelIntelChar
        .map(c =>
          db.query.map
            .upsertIntelCharacter(c)
            .tapError(ex => ZIO.logError(s"Failed to save character in db: $ex"))
            .ignore
        )
        .getOrElse(ZIO.unit)
    yield modelIntelChar) @@ Log.CharacterId(req.id)

  def layer: ZLayer[Env, Throwable, IntelDataSource] =
    ZLayer(apply())

  def apply(): ZIO[javax.sql.DataSource & EsiClient, Throwable, IntelDataSource] =
    for
      env <- ZIO.environment[Env]
      envD = Described(env, "intel query env")
      allianceSource <- DataCache
        .dataSource[Env, Throwable, GetAllianceById, model.Alliance](GetAllianceFromDbSource, getAllianceFromEsiAndSave)
      corpSource <- DataCache
        .dataSource[Env, Throwable, GetCorporationById, model.Corporation](
          GetCorporationFromDbSource,
          getCorporationFromEsiAndSave
        )
      charSource <- DataCache
        .dataSource[Env, Throwable, GetCharacterById, model.IntelCharacter](
          GetCharactersFromDbSource,
          getCharacterFromEsiAndSave
        )
    yield IntelDataSource(
      corpSource.provideEnvironment(envD),
      allianceSource.provideEnvironment(envD),
      charSource.provideEnvironment(envD)
    )

  private inline def optionalQueryIgnoreErrors[T, A](
      idOpt: Option[T],
      q: T => ZQuery[IntelDataSource, Throwable, Option[A]],
      msg: String
  ): URQuery[IntelDataSource, Option[A]] =
    idOpt.fold(ZQuery.none)(id => q(id).catchAllCauseZIO(c => ZIO.logErrorCause(msg, c).as(None)))

  def getCorporationById(id: CorporationId): ZQuery[IntelDataSource, Throwable, Option[model.Corporation]] =
    ZQuery.serviceWithQuery[IntelDataSource](ds => ZQuery.fromRequest(GetCorporationById(id))(ds.corporation))

  def getCorporationByIdOrNone(idOpt: Option[CorporationId]): URQuery[IntelDataSource, Option[model.Corporation]] =
    optionalQueryIgnoreErrors(idOpt, getCorporationById, "fetching corporation failed, ignoring")

  def getAllianceById(id: AllianceId): ZQuery[IntelDataSource, Throwable, Option[model.Alliance]] =
    ZQuery.serviceWithQuery[IntelDataSource](ds => ZQuery.fromRequest(GetAllianceById(id))(ds.alliance))

  def getAllianceByIdOrNone(idOpt: Option[AllianceId]): URQuery[IntelDataSource, Option[model.Alliance]] =
    optionalQueryIgnoreErrors(idOpt, getAllianceById, "fetching alliance failed, ignoring")

  def getCharacterById(id: CharacterId): ZQuery[IntelDataSource, Throwable, Option[model.IntelCharacter]] =
    ZQuery.serviceWithQuery[IntelDataSource](ds => ZQuery.fromRequest(GetCharacterById(id))(ds.character))

private def fromEsiCorporation(id: CorporationId, value: EsiCorporation, now: Instant): model.Corporation =
  model.Corporation(
    id = id,
    name = value.name,
    ticker = value.ticker,
    allianceId = value.allianceId,
    ceoCharacterId = value.ceoId,
    creatorCharacterId = value.creatorId,
    homeStationId = value.homeStationId,
    memberCount = value.memberCount,
    url = value.url,
    createdAt = value.dateFounded,
    updatedAt = now
  )

private def fromEsiAlliance(id: AllianceId, value: EsiAlliance, now: Instant): model.Alliance =
  model.Alliance(
    id = id,
    name = value.name,
    ticker = value.ticker,
    creatorCharacterId = value.creatorId,
    creatorCorporationId = value.creatorCorporationId,
    executorCorporationId = value.executorCorporationId,
    createdAt = value.dateFounded,
    updatedAt = now
  )

private def fromEsiCharacter(id: CharacterId, value: EsiCharacter, now: Instant): model.IntelCharacter =
  model.IntelCharacter(
    id = id,
    bloodlineId = value.bloodlineId,
    corporationId = value.corporationId,
    factionId = value.factionId,
    gender = value.gender,
    name = value.name,
    raceId = value.raceId,
    securityStatus = value.securityStatus,
    title = value.title,
    createdAt = value.birthday,
    updatedAt = now
  )
