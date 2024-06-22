package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.model.MapDisplayType
import org.updraft0.controltower.db.query.*
import zio.*

import java.time.Instant
import javax.sql.DataSource
import scala.annotation.nowarn

case class MapWormholeConnectionRank(
    connectionId: ConnectionId,
    fromSystemIdx: Int,
    fromSystemCount: Int,
    toSystemIdx: Int,
    toSystemCount: Int
)

case class MapWormholeConnectionWithSigs(
    connection: model.MapWormholeConnection,
    jumps: Chunk[model.MapWormholeConnectionJump],
    fromSignature: Option[model.MapSystemSignature],
    toSignature: Option[model.MapSystemSignature]
)

case class MapSystemWithAll(
    sys: model.MapSystem,
    display: Option[model.SystemDisplayData],
    structures: Chunk[model.MapSystemStructure],
    notes: Chunk[model.MapSystemNote],
    signatures: Chunk[model.MapSystemSignature],
    connections: Chunk[model.MapWormholeConnection]
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

  // opaque types
  given JsonDecoder[CharacterId] = JsonDecoder.long.map(CharacterId.apply)
  given JsonEncoder[CharacterId] = JsonEncoder.long.contramap(_.value)

  given JsonDecoder[CorporationId] = JsonDecoder.long.map(CorporationId.apply)
  given JsonEncoder[CorporationId] = JsonEncoder.long.contramap(_.value)

  given JsonDecoder[AllianceId] = JsonDecoder.long.map(AllianceId.apply)
  given JsonEncoder[AllianceId] = JsonEncoder.long.contramap(_.value)

  given JsonDecoder[SystemId] = JsonDecoder.long.map(SystemId.apply)
  given JsonEncoder[SystemId] = JsonEncoder.long.contramap(_.value)

  given JsonDecoder[SigId] = JsonDecoder.string.map(SigId.apply)
  given JsonEncoder[SigId] = JsonEncoder.string.contramap(_.convert)

  given JsonDecoder[MapId] = JsonDecoder.long.map(MapId.apply)
  given JsonEncoder[MapId] = JsonEncoder.long.contramap(_.value)

  given JsonDecoder[ConnectionId] = JsonDecoder.long.map(ConnectionId.apply)
  given JsonEncoder[ConnectionId] = JsonEncoder.long.contramap(_.value)

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
        systemId             <- m("systemId").as[SystemId]
        name                 <- m("name").as[String]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        ownerCorporationId   <- m("ownerCorporationId").as[Option[CorporationId]]
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
        mapId                <- m("mapId").as[MapId]
        systemId             <- m("systemId").as[SystemId]
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
        systemId             <- m("systemId").as[SystemId]
        signatureId          <- m("signatureId").as[SigId]
        isDeleted            <- m("isDeleted").as[Int].map(_ == 1)
        signatureGroup       <- m("signatureGroup").as[model.SignatureGroup]
        signatureTypeName    <- m("signatureTypeName").as[Option[String]]
        wormholeIsEol        <- m("wormholeIsEol").as[Option[Int]].map(_.map(_ == 1))
        wormholeEolAt        <- m("wormholeEolAt").as[Option[Long]].map(_.map(Instant.ofEpochMilli))
        wormholeTypeId       <- m("wormholeTypeId").as[Option[Long]]
        wormholeMassSize     <- m("wormholeMassSize").as[Option[model.WormholeMassSize]]
        wormholeMassStatus   <- m("wormholeMassStatus").as[Option[model.WormholeMassStatus]]
        wormholeK162Type     <- m("wormholeK162Type").as[Option[model.WormholeK162Type]]
        wormholeConnectionId <- m("wormholeConnectionId").as[Option[ConnectionId]]
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
        id                   <- m("id").as[ConnectionId]
        mapId                <- m("mapId").as[MapId]
        fromSystemId         <- m("fromSystemId").as[SystemId]
        toSystemId           <- m("toSystemId").as[SystemId]
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

  private given JsonDecoder[model.MapWormholeConnectionJump] = JsonDecoder
    .map[String, Json]
    .mapOrFail: m =>
      for
        connectionId <- m("connectionId").as[ConnectionId]
        characterId  <- m("characterId").as[CharacterId]
        shipTypeId   <- m("shipTypeId").as[Int]
        massOverride <- m("massOverride").as[Option[Int]]
        createdAt    <- m("createdAt").as[Long].map(Instant.ofEpochMilli)
      yield model.MapWormholeConnectionJump(connectionId, characterId, shipTypeId, massOverride, createdAt)

  def getMap(id: MapId): Result[Option[model.MapModel]] =
    run(quote {
      mapModel.filter(_.id == lift(id))
    }).map(_.headOption)

  def getMapsById(mapIds: List[MapId]): Result[List[model.MapModel]] =
    run(quote {
      mapModel.filter(m => liftQuery(mapIds).contains(m.id))
    })

  def getMapByCreatorUserAndName(userId: UserId, name: String): Result[Option[model.MapModel]] =
    run(quote {
      mapModel
        .filter(_.name == lift(name))
        .filter(_.creatorUserId == lift(userId))
    })
      .map(_.headOption)

  def createMap(userId: UserId, name: String, displayType: MapDisplayType): Result[MapId] =
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
  def getMapSystemAll(mapId: MapId, systemId: Option[SystemId] = None): Result[List[MapSystemWithAll]] =
    run(quote {
      (for
        map <- mapModel.filter(_.id == lift(mapId))
        sys <- mapSystem.join(ms => ms.mapId == map.id && lift(systemId).forall(_ == ms.systemId))
        dis <- mapSystemDisplay.join(sd =>
          sd.systemId == sys.systemId && sd.mapId == sys.mapId && sd.displayType == map.displayType
        )
        mss <- mapSystemStructure.leftJoin(ss => ss.systemId == sys.systemId && ss.mapId == sys.mapId && !ss.isDeleted)
        msn <- mapSystemNote.leftJoin(sn => sn.systemId == sys.systemId && sn.mapId == sys.mapId && !sn.isDeleted)
        msi <- mapSystemSignature.leftJoin(si => si.systemId == sys.systemId && si.mapId == sys.mapId && !si.isDeleted)
        mhc <- mapWormholeConnection.leftJoin(whc =>
          whc.mapId == map.id && (whc.fromSystemId == sys.systemId || whc.toSystemId == sys.systemId) && !whc.isDeleted
        )
      yield (sys, dis.data, mss, msn, msi, mhc)).groupByMap((ms, _, _, _, _, _) => (ms))(
        (ms, dis, mss, msn, msi, mhc) =>
          (
            ms,
            Some(dis),
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
        MapSystemWithAll(
          sys = mss,
          display = dis,
          structures = Chunk.fromArray(structures.value),
          notes = Chunk.fromArray(notes.value),
          signatures = Chunk.fromArray(signatures.value),
          connections = Chunk.fromArray(connections.value)
        )
      )
    )

  private inline def connectionsWithSigs(inline whcQ: EntityQuery[model.MapWormholeConnection]) =
    (for
      whc <- whcQ
      fromSig <- mapSystemSignature.leftJoin(mss =>
        mss.mapId == whc.mapId && mss.systemId == whc.fromSystemId && mss.wormholeConnectionId.contains(whc.id)
      )
      toSig <- mapSystemSignature.leftJoin(mss =>
        mss.mapId == whc.mapId && mss.systemId == whc.toSystemId && mss.wormholeConnectionId.contains(whc.id)
      )
      jumps <- mapWormholeConnectionJump.leftJoin(_.connectionId == whc.id)
    yield (whc, fromSig, toSig, jumps))
      .groupByMap((whc, _, _, _) => whc.id)((whc, fromSig, toSig, whcj) =>
        (
          whc,
          fromSig,
          toSig,
          jsonGroupArrayFilterNullDistinct[model.MapWormholeConnectionJump](
            jsonObject5(
              "connectionId",
              whcj.map(_.connectionId),
              "characterId",
              whcj.map(_.characterId),
              "shipTypeId",
              whcj.map(_.shipTypeId),
              "massOverride",
              whcj.map(_.massOverride),
              "createdAt",
              whcj.map(_.createdAt)
            ),
            whcj.map(_.connectionId)
          )
        )
      )

  def getWormholeConnectionsWithSigs(
      mapId: MapId,
      connectionIdOpt: Option[ConnectionId],
      includeDeleted: Boolean = false
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      quote(
        connectionsWithSigs(
          mapWormholeConnection
            .filter(whc =>
              whc.mapId == lift(mapId) && (lift(includeDeleted) || !whc.isDeleted) && lift(connectionIdOpt)
                .forall(_ == whc.id)
            )
        )
      )
    ).map(
      _.map((whc, fromSig, toSig, jumps) =>
        MapWormholeConnectionWithSigs(
          connection = whc,
          fromSignature = fromSig,
          toSignature = toSig,
          jumps = Chunk.fromArray(jumps.value)
        )
      )
    )

  def getWormholeConnectionsWithSigsBySystemId(
      mapId: MapId,
      systemId: SystemId
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      quote(
        connectionsWithSigs(
          mapWormholeConnection
            .filter(whc =>
              whc.mapId == lift(mapId) && !whc.isDeleted &&
                (whc.fromSystemId == lift(systemId) || whc.toSystemId == lift(systemId))
            )
        )
      )
    ).map(
      _.map((whc, fromSig, toSig, jumps) =>
        MapWormholeConnectionWithSigs(
          connection = whc,
          fromSignature = fromSig,
          toSignature = toSig,
          jumps = Chunk.fromArray(jumps.value)
        )
      )
    )

  def getWormholeConnectionsWithSigsBySystemIds(
      mapId: MapId,
      systemIds: Chunk[SystemId]
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      quote(
        connectionsWithSigs(
          mapWormholeConnection
            .filter(whc =>
              whc.mapId == lift(mapId) && !whc.isDeleted &&
                (liftQuery(systemIds).contains(whc.fromSystemId) || liftQuery(systemIds).contains(whc.toSystemId))
            )
        )
      )
    ).map(
      _.map((whc, fromSig, toSig, jumps) =>
        MapWormholeConnectionWithSigs(
          connection = whc,
          fromSignature = fromSig,
          toSignature = toSig,
          jumps = Chunk.fromArray(jumps.value)
        )
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
      quote(
        mapWormholeConnection.filter(whc => whc.mapId == lift(mapId) && !whc.isDeleted).map(ranksForConnection(_))
      )
    )

  def getWormholeConnectionRanksForSystem(
      mapId: MapId,
      systemId: SystemId
  ): Result[List[MapWormholeConnectionRank]] =
    run(
      quote(
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
      systemId1: SystemId,
      systemId2: SystemId
  ): Result[List[MapWormholeConnectionRank]] =
    run(
      quote(
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
  def deleteMap(mapId: MapId, userId: UserId): Result[Unit] =
    run(
      quote(
        mapModel
          .filter(_.id == lift(mapId))
          .update(_.deletedAt -> Some(unixepoch), _.deletedByUserId -> Some(lift(userId)))
      )
    ).unit
