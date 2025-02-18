package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.model.MapDisplayType
import org.updraft0.controltower.db.query.*
import org.updraft0.controltower.protocol.OpaqueCodecs
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
):
  lazy val systemIds: Set[SystemId] =
    (fromSignature, toSignature) match
      case (Some(from), Some(to)) => Set(from.systemId, to.systemId)
      case (Some(from), None)     => Set(from.systemId)
      case (None, Some(to))       => Set(to.systemId)
      case (None, None)           => Set.empty

case class MapSystemWithAll(
    sys: model.MapSystem,
    display: Option[model.SystemDisplayData],
    signatures: Chunk[model.MapSystemSignature],
    connections: Chunk[model.MapWormholeConnection],
    intel: Option[model.IntelSystem],
    notes: Chunk[model.IntelSystemNote],
    structures: Chunk[model.IntelSystemStructure],
    pings: Chunk[model.IntelSystemPing]
)

/** json decoders for json_array_agg usage (some logic duplicated between the MappedEntity and the codec here)
  */
private[db] trait MapQueryCodecs extends OpaqueCodecs:
  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  private[db] given [A <: AnyRef: scala.reflect.ClassTag: JsonValueCodec]: JsonValueCodec[Array[A]] =
    JsonCodecMaker.make

  private given JsonValueCodec[Boolean] = new JsonValueCodec[Boolean]:
    override def decodeValue(in: JsonReader, default: Boolean): Boolean = if in.readInt() == 1 then true else false
    override def encodeValue(x: Boolean, out: JsonWriter): Unit         = out.writeVal(if x then 1 else 0)
    override def nullValue: Boolean                                     = false

  private given JsonValueCodec[Instant] = new JsonValueCodec[Instant]:
    override def decodeValue(in: JsonReader, default: Instant): Instant = Instant.ofEpochMilli(in.readLong())
    override def encodeValue(x: Instant, out: JsonWriter): Unit         = out.writeVal(x.toEpochMilli)
    override def nullValue: Instant                                     = null

  private[db] given JsonValueCodec[model.SignatureGroup] = new JsonValueCodec[model.SignatureGroup]:
    override def decodeValue(in: JsonReader, default: model.SignatureGroup): model.SignatureGroup =
      model.SignatureGroup.fromOrdinal(in.readInt())
    override def encodeValue(x: model.SignatureGroup, out: JsonWriter): Unit = out.writeVal(x.ordinal())
    override def nullValue: model.SignatureGroup                             = null

  private[db] given JsonValueCodec[model.WormholeMassSize] = new JsonValueCodec[model.WormholeMassSize]:
    override def decodeValue(in: JsonReader, default: model.WormholeMassSize): model.WormholeMassSize =
      model.WormholeMassSize.fromOrdinal(in.readInt())
    override def encodeValue(x: model.WormholeMassSize, out: JsonWriter): Unit = out.writeVal(x.ordinal())
    override def nullValue: model.WormholeMassSize                             = null

  private[db] given JsonValueCodec[model.WormholeMassStatus] = new JsonValueCodec[model.WormholeMassStatus]:
    override def decodeValue(in: JsonReader, default: model.WormholeMassStatus): model.WormholeMassStatus =
      model.WormholeMassStatus.fromOrdinal(in.readInt())
    override def encodeValue(x: model.WormholeMassStatus, out: JsonWriter): Unit = out.writeVal(x.ordinal())
    override def nullValue: model.WormholeMassStatus                             = null

  private[db] given JsonValueCodec[model.WormholeK162Type] = new JsonValueCodec[model.WormholeK162Type]:
    override def decodeValue(in: JsonReader, default: model.WormholeK162Type): model.WormholeK162Type =
      model.WormholeK162Type.fromOrdinal(in.readInt())
    override def encodeValue(x: model.WormholeK162Type, out: JsonWriter): Unit = out.writeVal(x.ordinal())
    override def nullValue: model.WormholeK162Type                             = null

  private[db] given JsonValueCodec[model.IntelSystemNote]           = JsonCodecMaker.make
  private[db] given JsonValueCodec[model.IntelSystemPing]           = JsonCodecMaker.make
  private[db] given JsonValueCodec[model.IntelSystemStructure]      = JsonCodecMaker.make
  private[db] given JsonValueCodec[model.MapSystemSignature]        = JsonCodecMaker.make
  private[db] given JsonValueCodec[model.MapWormholeConnection]     = JsonCodecMaker.make
  private[db] given JsonValueCodec[model.MapWormholeConnectionJump] = JsonCodecMaker.make

/** Queries for map information
  */
object MapQueries extends MapQueryCodecs:
  import ctx.{*, given}
  import auth.given
  import map.given
  import map.schema.*

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
        msi <- mapSystemSignature.leftJoin(si => si.systemId == sys.systemId && si.mapId == sys.mapId && !si.isDeleted)
        mhc <- mapWormholeConnection.leftJoin(whc =>
          whc.mapId == map.id && (whc.fromSystemId == sys.systemId || whc.toSystemId == sys.systemId) && !whc.isDeleted
        )
        ins <- intelSystem.leftJoin(isy => isy.systemId == sys.systemId && isy.mapId == sys.mapId)
        isn <- intelSystemNote.leftJoin(isn => isn.systemId == sys.systemId && isn.mapId == sys.mapId && !isn.isDeleted)
        iss <- intelSystemStructure.leftJoin(ss =>
          ss.systemId == sys.systemId && ss.mapId == sys.mapId && !ss.isDeleted
        )
        isp <- intelSystemPing.leftJoin(sp => sp.systemId == sys.systemId && sp.mapId == sys.mapId && !sp.isDeleted)
      yield (sys, dis.data, msi, mhc, ins, isn, iss, isp)).groupByMap((ms, _, _, _, _, _, _, _) => (ms))(
        (ms, dis, msi, mhc, ins, isn, iss, isp) =>
          (
            ms,
            Some(dis),
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
            ),
            ins,
            jsonGroupArrayFilterNullDistinct[model.IntelSystemNote](
              jsonObject11(
                "id",
                isn.map(_.id),
                "mapId",
                isn.map(_.mapId),
                "systemId",
                isn.map(_.systemId),
                "note",
                isn.map(_.note),
                "isPinned",
                isn.map(_.isPinned),
                "isDeleted",
                isn.map(_.isDeleted),
                "originalId",
                isn.map(_.originalId),
                "createdAt",
                isn.map(_.createdAt),
                "createdByCharacterId",
                isn.map(_.createdByCharacterId),
                "deletedAt",
                isn.map(_.deletedAt),
                "deletedByCharacterId",
                isn.map(_.deletedByCharacterId)
              ),
              isn.map(_.id)
            ),
            jsonGroupArrayFilterNullDistinct[model.IntelSystemStructure](
              jsonObject16(
                "id",
                iss.map(_.id),
                "mapId",
                iss.map(_.mapId),
                "systemId",
                iss.map(_.systemId),
                "name",
                iss.map(_.name),
                "ownerCorporationId",
                iss.map(_.ownerCorporationId),
                "itemTypeId",
                iss.map(_.itemTypeId),
                "nearestPlanetIdx",
                iss.map(_.nearestPlanetIdx),
                "nearestMoonIdx",
                iss.map(_.nearestMoonIdx),
                "isOnline",
                iss.map(_.isOnline),
                "isDeleted",
                iss.map(_.isDeleted),
                "createdAt",
                iss.map(_.createdAt),
                "createdByCharacterId",
                iss.map(_.createdByCharacterId),
                "updatedAt",
                iss.map(_.updatedAt),
                "updatedByCharacterId",
                iss.map(_.updatedByCharacterId),
                "deletedAt",
                iss.map(_.deletedAt),
                "deletedByCharacterId",
                iss.map(_.deletedByCharacterId)
              ),
              iss.map(_.id)
            ),
            jsonGroupArrayFilterNullDistinct[model.IntelSystemPing](
              jsonObject11(
                "id",
                isp.map(_.id),
                "mapId",
                isp.map(_.mapId),
                "systemId",
                isp.map(_.systemId),
                "pingUserId",
                isp.map(_.pingUserId),
                "pingMapGlobal",
                isp.map(_.pingMapGlobal),
                "pingNote",
                isp.map(_.pingNote),
                "isDeleted",
                isp.map(_.isDeleted),
                "createdAt",
                isp.map(_.createdAt),
                "createdByCharacterId",
                isp.map(_.createdByCharacterId),
                "deletedAt",
                isp.map(_.deletedAt),
                "deletedByCharacterId",
                isp.map(_.deletedByCharacterId)
              ),
              isp.map(_.id)
            )
          )
      )
    }).map(
      _.map((mss, dis, signatures, connections, is, isn, iss, isp) =>
        MapSystemWithAll(
          sys = mss,
          display = dis,
          signatures = Chunk.fromArray(signatures.value),
          connections = Chunk.fromArray(connections.value),
          intel = is,
          notes = Chunk.fromArray(isn.value),
          structures = Chunk.fromArray(iss.value),
          pings = Chunk.fromArray(isp.value)
        )
      )
    )

  private inline def connectionsWithSigs(
      inline whcQ: EntityQuery[model.MapWormholeConnection],
      inline filterBy: (
          (
              model.MapWormholeConnection,
              Option[model.MapSystemSignature],
              Option[model.MapSystemSignature],
              Option[model.MapWormholeConnectionJump]
          )
      ) => Boolean = _ => true
  ) =
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
      .filter(filterBy)
      .sortBy((whc, _, _, whjs) => (whc.id, whjs.map(_.createdAt)))(Ord.asc)
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

  private inline def toWormholeConnectionsWithSigs(
      xs: List[
        (
            model.MapWormholeConnection,
            Option[model.MapSystemSignature],
            Option[model.MapSystemSignature],
            JsonValue[Array[model.MapWormholeConnectionJump]]
        )
      ]
  ) =
    xs.map((whc, fromSig, toSig, jumps) =>
      MapWormholeConnectionWithSigs(
        connection = whc,
        fromSignature = fromSig,
        toSignature = toSig,
        jumps = Chunk.fromArray(jumps.value)
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
              whc.mapId == lift(mapId) && infix"(${lift(includeDeleted) || !whc.isDeleted})".asCondition &&
                lift(connectionIdOpt).forall(_ == whc.id)
            )
        )
      )
    ).map(toWormholeConnectionsWithSigs)

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
    ).map(toWormholeConnectionsWithSigs)

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
    ).map(toWormholeConnectionsWithSigs)

  def getWormholeConnectionsWithSigsExpiredOrEol(
      mapId: MapId,
      normalExpiry: Duration,
      eolExpiry: Duration
  ): Result[List[MapWormholeConnectionWithSigs]] =
    run(
      quote(
        connectionsWithSigs(
          mapWormholeConnection.filter(whc => whc.mapId == lift(mapId) && !whc.isDeleted),
          (whc, fromSig, toSig, _) =>
            whc.createdAt < unixepochMinusSeconds(lift(normalExpiry.toSeconds)) ||
              // note: quill does not seem to be able to group the conditions properly together - help it with explicit brackets
              infix"(${fromSig.map(_.signatureId).isDefined && fromSig.orNull.wormholeEolAt
                  .exists(_ < unixepochMinusSeconds(lift(eolExpiry.toSeconds)))})".asCondition ||
              infix"(${toSig.map(_.signatureId).isDefined && toSig.orNull.wormholeEolAt
                  .exists(_ < unixepochMinusSeconds(lift(eolExpiry.toSeconds)))})".asCondition
        )
      )
    ).map(toWormholeConnectionsWithSigs)

  def getHardDeleteConnectionIds(mapId: MapId, expiry: Duration): Result[List[ConnectionId]] =
    run(
      quote(
        mapWormholeConnection
          .filter(whc =>
            whc.mapId == lift(mapId) && whc.isDeleted && whc.updatedAt < unixepochMinusSeconds(lift(expiry.toSeconds))
          )
          .map(_.id)
      )
    )

  def getHardDeleteSignatures(mapId: MapId, expiry: Duration): Result[List[model.MapSystemSignature]] =
    run(
      quote(
        mapWormholeConnection
          .filter(whc =>
            whc.mapId == lift(mapId) && whc.isDeleted && whc.updatedAt < unixepochMinusSeconds(lift(expiry.toSeconds))
          )
          .join(mapSystemSignature)
          .on((whc, mss) => mss.wormholeConnectionId.contains(whc.id))
          .map(_._2)
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
