package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.model.MapDisplayType
import org.updraft0.controltower.db.query.*
import zio.*

import java.time.Instant
import javax.sql.DataSource
import scala.annotation.nowarn

case class MapWormholeConnectionRank(
    connectionId: Long,
    fromSystemIdx: Int,
    fromSystemCount: Int,
    toSystemIdx: Int,
    toSystemCount: Int
)

case class MapWormholeConnectionWithSigs(
    connection: model.MapWormholeConnection,
    fromSignature: Option[model.MapSystemSignature],
    toSignature: Option[model.MapSystemSignature]
)

case class MapSystemWithAll(
    sys: model.MapSystem,
    display: Option[model.SystemDisplayData],
    structures: Array[model.MapSystemStructure],
    notes: Array[model.MapSystemNote],
    signatures: Array[model.MapSystemSignature],
    connections: Array[model.MapWormholeConnection]
)

/** Queries for map information
  */
object MapQueries:
  import ctx.{*, given}
  import auth.given
  import map.given
  import map.schema.*
  import zio.json.*
  import zio.json.ast.Json

  given JsonDecoder[CharacterId] = JsonDecoder.long.map(CharacterId.apply)
  given JsonEncoder[CharacterId] = JsonEncoder.long.contramap(_.asInstanceOf[Long])

  // json decoders for json_array_agg usage (some logic duplicated between the MappedEntity and the codec here)
  private given JsonDecoder[model.SignatureGroup]     = JsonDecoder.int.map(model.SignatureGroup.fromOrdinal)
  private given JsonDecoder[model.WormholeMassSize]   = JsonDecoder.int.map(model.WormholeMassSize.fromOrdinal)
  private given JsonDecoder[model.WormholeMassStatus] = JsonDecoder.int.map(model.WormholeMassStatus.fromOrdinal)
  private given JsonDecoder[model.WormholeK162Type]   = JsonDecoder.int.map(model.WormholeK162Type.fromOrdinal)

  // TODO either make zio-json support custom decoders or give up and use jsoniter here
  private given JsonDecoder[model.MapSystemStructure] = JsonDecoder
    .map[String, Json]
    .mapOrFail: m =>
      for
        mapId                <- m("mapId").as[MapId]
        systemId             <- m("systemId").as[model.SystemId]
        name                 <- m("name").as[String]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        ownerCorporationId   <- m("ownerCorporationId").as[Option[model.CorporationId]]
        structureType        <- m("structureType").as[Option[String]]
        location             <- m("location").as[Option[String]]
        createdAt            <- m("createdAt").as[Long].map(Instant.ofEpochMilli)
        createdByCharacterId <- m("createdByCharacterId").as[CharacterId]
        updatedAt            <- m("updatedAt").as[Long].map(Instant.ofEpochMilli)
        updatedByCharacterId <- m("updatedByCharacterId").as[CharacterId]
      yield model.MapSystemStructure(
        mapId,
        systemId,
        name,
        isDeleted,
        ownerCorporationId,
        structureType,
        location,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId
      )

  private given JsonDecoder[model.MapSystemNote] = JsonDecoder
    .map[String, Json]
    .mapOrFail: m =>
      for
        id                   <- m("id").as[Long]
        mapId                <- m("mapId").as[Long]
        systemId             <- m("systemId").as[model.SystemId]
        note                 <- m("note").as[String]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        createdAt            <- m("createdAt").as[Long].map(Instant.ofEpochMilli)
        createdByCharacterId <- m("createdByCharacterId").as[CharacterId]
        updatedAt            <- m("updatedAt").as[Long].map(Instant.ofEpochMilli)
        updatedByCharacterId <- m("updatedByCharacterId").as[CharacterId]
      yield model.MapSystemNote(
        id,
        mapId,
        systemId,
        note,
        isDeleted,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId
      )
  private given JsonDecoder[model.MapSystemSignature] = JsonDecoder
    .map[String, Json]
    .mapOrFail: m =>
      for
        mapId                <- m("mapId").as[MapId]
        systemId             <- m("systemId").as[model.SystemId]
        signatureId          <- m("signatureId").as[String]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        signatureGroup       <- m("signatureGroup").as[model.SignatureGroup]
        signatureTypeName    <- m("signatureTypeName").as[Option[String]]
        wormholeIsEol        <- m("wormholeIsEol").as[Option[Int]].map(_.map(_ == 1))
        wormholeEolAt        <- m("wormholeEolAt").as[Option[Long]].map(_.map(Instant.ofEpochMilli))
        wormholeTypeId       <- m("wormholeTypeId").as[Option[Long]]
        wormholeMassSize     <- m("wormholeMassSize").as[Option[model.WormholeMassSize]]
        wormholeMassStatus   <- m("wormholeMassStatus").as[Option[model.WormholeMassStatus]]
        wormholeK162Type     <- m("wormholeK162Type").as[Option[model.WormholeK162Type]]
        wormholeConnectionId <- m("wormholeConnectionId").as[Option[Long]]
        createdAt            <- m("createdAt").as[Long].map(Instant.ofEpochMilli)
        createdByCharacterId <- m("createdByCharacterId").as[CharacterId]
        updatedAt            <- m("updatedAt").as[Long].map(Instant.ofEpochMilli)
        updatedByCharacterId <- m("updatedByCharacterId").as[CharacterId]
      yield model.MapSystemSignature(
        mapId,
        systemId,
        signatureId,
        isDeleted,
        signatureGroup,
        signatureTypeName,
        wormholeIsEol,
        wormholeEolAt,
        wormholeTypeId,
        wormholeMassSize,
        wormholeMassStatus,
        wormholeK162Type,
        wormholeConnectionId,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId
      )
  private given JsonDecoder[model.MapWormholeConnection] = JsonDecoder
    .map[String, Json]
    .mapOrFail: m =>
      for
        id                   <- m("id").as[Long]
        mapId                <- m("mapId").as[MapId]
        fromSystemId         <- m("fromSystemId").as[Long]
        toSystemId           <- m("toSystemId").as[Long]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        createdAt            <- m("createdAt").as[Long].map(Instant.ofEpochMilli)
        createdByCharacterId <- m("createdByCharacterId").as[CharacterId]
        updatedAt            <- m("updatedAt").as[Long].map(Instant.ofEpochMilli)
        updatedByCharacterId <- m("updatedByCharacterId").as[CharacterId]
      yield model.MapWormholeConnection(
        id,
        mapId,
        fromSystemId,
        toSystemId,
        isDeleted,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId
      )

  type MapId = Long

  def getMap(id: MapId): Result[Option[model.MapModel]] =
    run(quote {
      mapModel.filter(_.id == lift(id))
    }).map(_.headOption)

  def getMapsById(mapIds: List[MapId]): Result[List[model.MapModel]] =
    run(quote {
      mapModel.filter(m => liftQuery(mapIds).contains(m.id))
    })

  def getMapByCreatorUserAndName(userId: Long, name: String): Result[Option[model.MapModel]] =
    run(quote {
      mapModel
        .filter(_.name == lift(name))
        .filter(_.creatorUserId == lift(userId))
    })
      .map(_.headOption)

  def createMap(userId: Long, name: String, displayType: MapDisplayType): Result[MapId] =
    run(quote {
      mapModel
        .insert(_.creatorUserId -> lift(userId), _.name -> lift(name), _.displayType -> lift(displayType))
        .returning(_.id)
    })

  def updateMap(mapId: MapId, name: String, displayType: MapDisplayType): Result[Long] =
    run(
      quote(
        mapModel
          .filter(_.id == lift(mapId))
          .update(_.name -> lift(name), _.displayType -> lift(displayType))
      )
    )

  def getMapNamesByIds(ids: List[MapId]): Result[Map[MapId, String]] =
    run(quote {
      mapModel
        .filter(m => liftQuery(ids).contains(m.id))
        .map(m => (m.id, m.name))
    }).map(_.toMap)

  @nowarn("msg=.*it is preferable to define both an encoder and a decoder.*")
  def getMapSystemAll(mapId: MapId, systemId: Option[Long] = None): Result[List[MapSystemWithAll]] =
    run(quote {
      (for
        map <- mapModel.filter(_.id == lift(mapId))
        sys <- mapSystem.join(ms => ms.mapId == map.id && lift(systemId).forall(_ == ms.systemId))
        dis <- mapSystemDisplay.leftJoin(sd =>
          sd.systemId == sys.systemId && sd.mapId == sys.mapId && sd.displayType == map.displayType
        )
        mss <- mapSystemStructure.leftJoin(ss => ss.systemId == sys.systemId && ss.mapId == sys.mapId && !ss.isDeleted)
        msn <- mapSystemNote.leftJoin(sn => sn.systemId == sys.systemId && sn.mapId == sys.mapId && !sn.isDeleted)
        msi <- mapSystemSignature.leftJoin(si => si.systemId == sys.systemId && si.mapId == sys.mapId && !si.isDeleted)
        mhc <- mapWormholeConnection.leftJoin(whc =>
          whc.mapId == map.id && (whc.fromSystemId == sys.systemId || whc.toSystemId == sys.systemId) && !whc.isDeleted
        )
      yield (sys, dis.map(_.data), mss, msn, msi, mhc)).groupByMap((ms, _, _, _, _, _) => (ms))(
        (ms, dis, mss, msn, msi, mhc) =>
          (
            ms,
            dis,
            jsonGroupArrayFilterNullDistinct[model.MapSystemStructure](
              jsonObject11(
                "mapId",
                mss.map(_.mapId),
                "systemId",
                mss.map(_.systemId),
                "name",
                mss.map(_.name),
                "isDeleted",
                mss.map(_.isDeleted),
                "ownerCorporationId",
                mss.map(_.ownerCorporationId),
                "structureType",
                mss.map(_.structureType),
                "location",
                mss.map(_.location),
                "createdAt",
                mss.map(_.createdAt),
                "createdByCharacterId",
                mss.map(_.createdByCharacterId),
                "updatedAt",
                mss.map(_.updatedAt),
                "updatedByCharacterId",
                mss.map(_.updatedByCharacterId)
              ),
              mss.map(_.name)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapSystemNote](
              jsonObject9(
                "id",
                msn.map(_.id),
                "mapId",
                msn.map(_.mapId),
                "systemId",
                msn.map(_.systemId),
                "note",
                msn.map(_.note),
                "isDeleted",
                msn.map(_.isDeleted),
                "createdAt",
                msn.map(_.createdAt),
                "createdByCharacterId",
                msn.map(_.createdByCharacterId),
                "updatedAt",
                msn.map(_.updatedAt),
                "updatedByCharacterId",
                msn.map(_.updatedByCharacterId)
              ),
              msn.map(_.id)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapSystemSignature](
              jsonObject17(
                "mapId",
                msi.map(_.mapId),
                "systemId",
                msi.map(_.systemId),
                "signatureId",
                msi.map(_.signatureId),
                "isDeleted",
                msi.map(_.isDeleted),
                "signatureGroup",
                msi.map(_.signatureGroup),
                "signatureTypeName",
                msi.map(_.signatureTypeName),
                "wormholeIsEol",
                msi.map(_.wormholeIsEol),
                "wormholeEolAt",
                msi.map(_.wormholeEolAt),
                "wormholeTypeId",
                msi.map(_.wormholeTypeId),
                "wormholeMassSize",
                msi.map(_.wormholeMassSize),
                "wormholeMassStatus",
                msi.map(_.wormholeMassStatus),
                "wormholeK162Type",
                msi.map(_.wormholeK162Type),
                "wormholeConnectionId",
                msi.map(_.wormholeConnectionId),
                "createdAt",
                msi.map(_.createdAt),
                "createdByCharacterId",
                msi.map(_.createdByCharacterId),
                "updatedAt",
                msi.map(_.updatedAt),
                "updatedByCharacterId",
                msi.map(_.updatedByCharacterId)
              ),
              msi.map(_.signatureId)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapWormholeConnection](
              jsonObject9(
                "id",
                mhc.map(_.id),
                "mapId",
                mhc.map(_.mapId),
                "fromSystemId",
                mhc.map(_.fromSystemId),
                "toSystemId",
                mhc.map(_.toSystemId),
                "isDeleted",
                mhc.map(_.isDeleted),
                "createdAt",
                mhc.map(_.createdAt),
                "createdByCharacterId",
                mhc.map(_.createdByCharacterId),
                "updatedAt",
                mhc.map(_.updatedAt),
                "updatedByCharacterId",
                mhc.map(_.updatedByCharacterId)
              ),
              mhc.map(_.id)
            )
          )
      )
    }).map(
      _.map((mss, dis, structures, notes, signatures, connections) =>
        MapSystemWithAll(mss, dis, structures.value, notes.value, signatures.value, connections.value)
      )
    )

  private inline def connectionsWithSigs(inline whc: model.MapWormholeConnection) =
    for
      fromSig <- mapSystemSignature.leftJoin(mss =>
        mss.mapId == whc.mapId && mss.systemId == whc.fromSystemId && mss.wormholeConnectionId.contains(whc.id)
      )
      toSig <- mapSystemSignature.leftJoin(mss =>
        mss.mapId == whc.mapId && mss.systemId == whc.toSystemId && mss.wormholeConnectionId.contains(whc.id)
      )
    yield MapWormholeConnectionWithSigs(
      connection = whc,
      fromSignature = fromSig,
      toSignature = toSig
    )

  def getWormholeConnectionsWithSigs(
      mapId: MapId,
      connectionIdOpt: Option[Long],
      includeDeleted: Boolean = false
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      autoQuote(
        mapWormholeConnection
          .filter(whc =>
            whc.mapId == lift(mapId) && (lift(includeDeleted) || !whc.isDeleted) && lift(connectionIdOpt)
              .forall(_ == whc.id)
          )
          .flatMap(connectionsWithSigs(_))
      )
    )

  def getWormholeConnectionsWithSigsBySystemId(
      mapId: MapId,
      systemId: Long
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      autoQuote(
        mapWormholeConnection
          .filter(whc =>
            whc.mapId == lift(mapId) && !whc.isDeleted &&
              (whc.fromSystemId == lift(systemId) || whc.toSystemId == lift(systemId))
          )
          .flatMap(connectionsWithSigs(_))
      )
    )

  private inline def ranksForConnection(inline whc: model.MapWormholeConnection): MapWormholeConnectionRank =
    val toRank = infix"rank() over (partition by ${whc.mapId}, ${whc.toSystemId} order by ${whc.id} asc)".pure.as[Int]
    val toCount = mapWormholeConnection
      .filter(mwc => mwc.mapId == whc.mapId && !mwc.isDeleted && mwc.toSystemId == whc.toSystemId)
      .map(_ => 1)
      .nested
      .size
    val fromRank = infix"rank() over (partition by ${whc.mapId}, ${whc.fromSystemId} order by ${whc.id} asc)".pure
      .as[Int]
    val fromCount = mapWormholeConnection
      .filter(mwc => mwc.mapId == whc.mapId && !mwc.isDeleted && mwc.fromSystemId == whc.fromSystemId)
      .map(_ => 1)
      .nested
      .size
    MapWormholeConnectionRank(whc.id, fromRank, fromCount.toInt, toRank, toCount.toInt)

  def getWormholeConnectionRanksAll(mapId: MapId): Result[List[MapWormholeConnectionRank]] =
    run(
      autoQuote(
        mapWormholeConnection.filter(whc => whc.mapId == lift(mapId) && !whc.isDeleted).map(ranksForConnection(_))
      )
    )

  def getWormholeConnectionRanksForSystem(mapId: MapId, systemId: Long): Result[List[MapWormholeConnectionRank]] =
    run(
      autoQuote(
        for
          allRanks <- mapWormholeConnection
            .filter(whc => whc.mapId == lift(mapId) && !whc.isDeleted)
            .map(ranksForConnection(_))
          _ <- mapWormholeConnection.join(whc =>
            whc.id == allRanks.connectionId && (whc.fromSystemId == lift(systemId) || whc.toSystemId == lift(systemId))
          )
        yield allRanks
      )
    )

  def getWormholeConnectionRanksForSystems(
      mapId: MapId,
      systemId1: Long,
      systemId2: Long
  ): Result[List[MapWormholeConnectionRank]] =
    run(
      autoQuote(
        for
          allRanks <- mapWormholeConnection
            .filter(whc => whc.mapId == lift(mapId) && !whc.isDeleted)
            .map(ranksForConnection(_))
          _ <- mapWormholeConnection.join(whc =>
            whc.id == allRanks.connectionId && (whc.fromSystemId == lift(systemId1) || whc.fromSystemId ==
              lift(systemId2) || whc.toSystemId == lift(systemId1) || whc.toSystemId == lift(systemId2))
          )
        yield allRanks
      )
    )

  // deletes
  def deleteMap(mapId: MapId, userId: Long): Result[Unit] =
    run(
      quote(
        mapModel
          .filter(_.id == lift(mapId))
          .update(_.deletedAt -> Some(unixepoch), _.deletedByUserId -> Some(lift(userId)))
      )
    ).unit
