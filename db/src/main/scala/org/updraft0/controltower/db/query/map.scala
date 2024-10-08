package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.*
import zio.{ZIO, Chunk}

import scala.collection.immutable.BitSet
import java.time.Instant

object map:
  import auth.given
  import schema.*
  import ctx.{*, given}
  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  private val BatchRows = 5_000

  private val WormholeGroupId = 988L

  private inline def config: CodecMakerConfig = CodecMakerConfig.withDiscriminatorFieldName(None)

  // this is always stored as json in the db
  given JsonValueCodec[SystemDisplayData]   = JsonCodecMaker.make(config)
  given JsonValueCodec[Map[String, Double]] = JsonCodecMaker.make(config)

  given MappedEncoding[Int, WormholeClass] = MappedEncoding(WormholeClasses.ById.apply)
  given MappedEncoding[WormholeClass, Int] = MappedEncoding(_.value)

  given MappedEncoding[Int, ChainNamingStrategy] = MappedEncoding(ChainNamingStrategy.fromOrdinal)
  given MappedEncoding[ChainNamingStrategy, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, IntelStance] = MappedEncoding(IntelStance.fromOrdinal)
  given MappedEncoding[IntelStance, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, MapDisplayType] = MappedEncoding(MapDisplayType.fromOrdinal)
  given MappedEncoding[MapDisplayType, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, SignatureGroup] = MappedEncoding(SignatureGroup.fromOrdinal)
  given MappedEncoding[SignatureGroup, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, WormholeMassSize] = MappedEncoding(WormholeMassSize.fromOrdinal)
  given MappedEncoding[WormholeMassSize, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, WormholeMassStatus] = MappedEncoding(WormholeMassStatus.fromOrdinal)
  given MappedEncoding[WormholeMassStatus, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, WormholeK162Type] = MappedEncoding(WormholeK162Type.fromOrdinal)
  given MappedEncoding[WormholeK162Type, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[String, SystemDisplayData] = MappedEncoding(readFromString[SystemDisplayData](_))
  given MappedEncoding[SystemDisplayData, String] = MappedEncoding(writeToString[SystemDisplayData](_))

  given MappedEncoding[Int, Set[WormholeClass]] =
    MappedEncoding(i => whClassesFromSet(BitSet.fromBitMaskNoCopy(Array(i.toLong))))
  given MappedEncoding[Set[WormholeClass], Int] = MappedEncoding(s => BitSet(s.map(_.value).toSeq*).toBitMask(0).toInt)

  /** Each table lives in the `map` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def mapModel              = quote(querySchema[MapModel]("map.map"))
    inline def mapSystem             = quote(querySchema[MapSystem]("map.map_system"))
    inline def mapSystemStructure    = quote(querySchema[MapSystemStructure]("map.map_system_structure"))
    inline def mapSystemNote         = quote(querySchema[MapSystemNote]("map.map_system_note"))
    inline def mapSystemDisplay      = quote(querySchema[MapSystemDisplay]("map.map_system_display"))
    inline def mapWormholeConnection = quote(querySchema[MapWormholeConnection]("map.map_wormhole_connection"))
    inline def mapWormholeConnectionJump = quote(
      querySchema[MapWormholeConnectionJump]("map.map_wormhole_connection_jump")
    )
    inline def mapSystemSignature   = quote(querySchema[MapSystemSignature]("map.map_system_signature"))
    inline def alliance             = quote(querySchema[Alliance]("map.alliance"))
    inline def corporation          = quote(querySchema[Corporation]("map.corporation"))
    inline def systemStaticWormhole = quote(querySchema[SystemStaticWormhole]("map.ref_system_static_wormhole"))
    inline def wormhole             = quote(querySchema[Wormhole]("map.ref_wormhole"))
    inline def signatureInGroup     = quote(querySchema[SignatureInGroup]("map.ref_signature_in_group"))

  // queries
  def getWormholesUsingTypeDogma: DbOperation[List[(Long, String, JsonValue[Map[String, Double]])]] =
    ctx.run(quote {
      (for
        it  <- sde.schema.itemType.filter(_.groupId == lift(WormholeGroupId))
        ida <- sde.schema.itemDogmaAttribute.join(_.itemId == it.id)
        dat <- sde.schema.dogmaAttributeType.join(_.id == ida.attributeId)
      yield (it, ida, dat)) // NOTE: the group by it.name is important because Wormhole C729 has multiple entries
        .groupByMap((it, _, _) => (it.id, it.name))((it, ida, dat) =>
          (it.id, it.name, jsonGroupObject(dat.name, ida.value))
        )
    })

  def getWormholeConnection(mapId: MapId, connectionId: ConnectionId): DbOperation[Option[MapWormholeConnection]] =
    ctx
      .run(quote { mapWormholeConnection.filter(whc => whc.mapId == lift(mapId) && whc.id == lift(connectionId)) })
      .map(_.headOption)

  def getWormholeConnections(
      mapId: MapId,
      connectionIds: Chunk[ConnectionId],
      isDeleted: Boolean
  ): DbOperation[List[MapWormholeConnection]] =
    ctx.run(
      quote(
        mapWormholeConnection.filter(whc =>
          whc.mapId == lift(mapId) && whc.isDeleted == lift(isDeleted) && liftQuery(connectionIds).contains(whc.id)
        )
      )
    )

  def getWormholeSystemNames: DbOperation[Map[String, SystemId]] =
    ctx
      .run(quote(sde.schema.solarSystem.map(ss => ss.name -> ss.id)))
      .map(_.filter((n, _) => n.startsWith("J") || n == "Thera" || n == "Turnur").toMap)

  def getWormholeTypeNames: DbOperation[List[(String, Long)]] =
    ctx.run(quote { sde.schema.itemType.filter(_.groupId == lift(WormholeGroupId)).map(it => it.name -> it.id) })

  def getMapSystem(mapId: MapId, systemId: SystemId): DbOperation[Option[MapSystem]] =
    ctx
      .run(quote(mapSystem.filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId))))
      .map(_.headOption)

  private inline def findWormholeMapSignatures(mapId: MapId, systemId: SystemId) =
    quote(
      mapSystemSignature
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .filter(_.isDeleted == lift(false))
        .filter(_.wormholeConnectionId.nonEmpty)
    )

  private inline def joinConnectionIds(inline mssq: Quoted[Query[MapSystemSignature]]) =
    mssq
      .join(mapWormholeConnection)
      .on((mss, mwc) => mss.wormholeConnectionId.contains(mwc.id) && mwc.isDeleted == lift(false))
      .map((_, mwc) => mwc.id)

  def getSystemConnectionIdsInSignatures(
      mapId: MapId,
      systemId: SystemId,
      sigIds: Option[Chunk[SigId]]
  ): DbOperation[List[ConnectionId]] =
    sigIds match
      case Some(sigIds) =>
        ctx.run(
          joinConnectionIds(
            findWormholeMapSignatures(mapId, systemId).filter(mss => liftQuery(sigIds).contains(mss.signatureId))
          )
        )
      case None =>
        ctx.run(joinConnectionIds(findWormholeMapSignatures(mapId, systemId)))

  // inserts
  def insertMapWormholeConnection(value: MapWormholeConnection): DbOperation[MapWormholeConnection] =
    ctx
      .run(quote {
        mapWormholeConnection
          .insertValue(lift(value))
          .returningGenerated(_.id)
      })
      .map(newId => value.copy(id = newId))

  def insertMapWormholeConnectionJump(value: MapWormholeConnectionJump): DbOperation[Long] =
    ctx.run(quote(mapWormholeConnectionJump.insertValue(lift(value))))

  // upserts
  def upsertMap(value: MapModel): DbOperation[Long] =
    ctx.run(quote {
      mapModel
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.name -> e.name,
          (t, e) => t.displayType -> e.displayType
        )
    })

  def upsertMapSystem(value: MapSystem): DbOperation[Long] =
    ctx.run(quote {
      mapSystem
        .insertValue(lift(value))
        .onConflictUpdate(_.mapId, _.systemId)(
          (t, e) => t.name -> e.name,
          (t, e) => t.isPinned -> e.isPinned,
          (t, e) => t.chainNamingStrategy -> e.chainNamingStrategy,
          (t, e) => t.description -> e.description,
          (t, e) => t.stance -> e.stance,
          (t, e) => t.updatedAt -> e.updatedAt,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    })

  def updateMapSystemName(
      mapId: MapId,
      systemId: SystemId,
      name: Option[String],
      updatedAt: Instant,
      updatedByCharacterId: CharacterId
  ) =
    ctx.run(quote {
      mapSystem
        .filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId))
        .update(
          _.name                 -> lift(name),
          _.updatedAt            -> lift(updatedAt),
          _.updatedByCharacterId -> lift(updatedByCharacterId)
        )
    })

  def updateMapAttribute(
      mapId: MapId,
      systemId: SystemId,
      isPinned: Option[Boolean],
      intelStance: Option[IntelStance],
      updatedAt: Instant,
      updatedByCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(quote {
      mapSystem
        .filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId))
        .update(
          ms => ms.stance -> lift(intelStance).getOrElse(ms.stance),
          ms => ms.isPinned -> lift(isPinned).getOrElse(ms.isPinned),
          _.updatedAt            -> lift(updatedAt),
          _.updatedByCharacterId -> lift(updatedByCharacterId)
        )
    })

  def upsertMapSystemDisplay(value: MapSystemDisplay): DbOperation[Long] =
    ctx.run(
      quote {
        mapSystemDisplay
          .insertValue(lift(value))
          .onConflictUpdate(_.mapId, _.systemId)(
            (t, e) => t.displayType -> e.displayType,
            (t, e) => t.data -> e.data
          )
      }
    )

  def upsertMapSystemStructure(value: MapSystemStructure): DbOperation[Long] =
    ctx.run(
      quote {
        mapSystemStructure
          .insertValue(lift(value))
          .onConflictUpdate(_.mapId, _.systemId, _.name)(
            (t, e) => t.isDeleted -> e.isDeleted,
            (t, e) => t.ownerCorporationId -> e.ownerCorporationId,
            (t, e) => t.structureType -> e.structureType,
            (t, e) => t.location -> e.location,
            (t, e) => t.updatedAt -> e.updatedAt,
            (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
          )
      }
    )

  def upsertMapSystemNote(value: MapSystemNote): DbOperation[Long] =
    ctx.run(quote {
      mapSystemNote
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.note -> e.note,
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, e) => t.updatedAt -> e.updatedAt,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    })

  def upsertMapWormholeConnection(value: MapWormholeConnection): DbOperation[Long] =
    ctx.run(
      mapWormholeConnection
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, e) => t.updatedAt -> e.updatedAt,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def upsertMapSystemSignature(value: MapSystemSignature): DbOperation[Long] =
    ctx.run(
      mapSystemSignature
        .insertValue(lift(value))
        .onConflictUpdate(_.mapId, _.systemId, _.signatureId)(
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, e) => t.signatureGroup -> e.signatureGroup,
          (t, e) => t.signatureTypeName -> e.signatureTypeName,
          (t, e) => t.wormholeIsEol -> e.wormholeIsEol,
          (t, e) => t.wormholeEolAt -> e.wormholeEolAt,
          (t, e) => t.wormholeTypeId -> e.wormholeTypeId,
          (t, e) => t.wormholeMassSize -> e.wormholeMassSize,
          (t, e) => t.wormholeMassStatus -> e.wormholeMassStatus,
          (t, e) => t.wormholeK162Type -> e.wormholeK162Type,
          (t, e) => t.wormholeConnectionId -> e.wormholeConnectionId,
          (t, e) => t.updatedAt -> e.updatedAt,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def upsertSystemStatics(values: List[SystemStaticWormhole]): DbOperation[Long] =
    ctx
      .run(
        liftQuery(values)
          .foreach(v => systemStaticWormhole.insertValue(v).onConflictIgnore(_.systemId, _.staticTypeId, _.validFrom)),
        BatchRows
      )
      .map(_.sum)

  def upsertSignaturesInGroup(values: List[SignatureInGroup]): DbOperation[Long] =
    ctx
      .run(
        liftQuery(values).foreach(v => signatureInGroup.insertValue(v).onConflictIgnore(_.name, _.signatureGroup)),
        BatchRows
      )
      .map(_.sum)

  def upsertWormholes(values: List[Wormhole]): DbOperation[Long] =
    ctx.run(liftQuery(values).foreach(v => wormhole.insertValue(v).onConflictIgnore(_.name)), BatchRows).map(_.sum)

  // deletes
  def deleteMapSystemDisplay(mapId: MapId, systemId: SystemId): DbOperation[Long] =
    ctx.run(
      mapSystemDisplay
        .filter(msd => msd.mapId == lift(mapId) && msd.systemId == lift(systemId))
        .delete
    )

  def deleteMapSystemDisplays(mapId: MapId, systemIds: Chunk[SystemId]): DbOperation[Long] =
    ctx.run(
      mapSystemDisplay
        .filter(msd => msd.mapId == lift(mapId) && liftQuery(systemIds).contains(msd.systemId))
        .delete
    )

  def deleteMapWormholeConnection(mapId: MapId, id: ConnectionId, byCharacterId: CharacterId): DbOperation[Long] =
    ctx.run(
      mapWormholeConnection
        .filter(_.id == lift(id))
        .filter(_.mapId == lift(mapId))
        .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
    )

  def deleteMapWormholeConnections(
      mapId: MapId,
      ids: Chunk[ConnectionId],
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx
      .run(
        quote(
          liftQuery(ids).foreach(id =>
            mapWormholeConnection
              .filter(_.id == id)
              .filter(_.mapId == lift(mapId))
              .update(
                _.isDeleted            -> lift(true),
                _.updatedByCharacterId -> lift(byCharacterId),
                _.updatedAt            -> unixepoch
              )
          )
        )
      )
      .map(_.sum)

  def deleteSignaturesWithConnectionIds(
      mapId: MapId,
      ids: Chunk[ConnectionId],
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx
      .run(
        quote(
          liftQuery(ids).foreach(id =>
            mapSystemSignature
              .filter(_.mapId == lift(mapId))
              .filter(_.wormholeConnectionId.exists(_ == id))
              .update(
                _.isDeleted            -> lift(true),
                _.updatedByCharacterId -> lift(byCharacterId),
                _.updatedAt            -> unixepoch
              )
          )
        )
      )
      .map(_.sum)

  def deleteMapSystemSignaturesAll(
      mapId: MapId,
      systemId: SystemId,
      now: Instant,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      quote(
        mapSystemSignature
          .filter(_.mapId == lift(mapId))
          .filter(_.systemId == lift(systemId))
          .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> lift(now))
      )
    )

  def deleteMapSystemSignatures(
      mapId: MapId,
      systemId: SystemId,
      signatureIds: Chunk[SigId],
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx
      .run(
        liftQuery(signatureIds).foreach(sigId =>
          mapSystemSignature
            .filter(_.mapId == lift(mapId))
            .filter(_.systemId == lift(systemId))
            .filter(_.signatureId == sigId)
            .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
        )
      )
      .map(_.sum)

  def deleteMapSystemNote(id: Long, byCharacterId: CharacterId): DbOperation[Long] =
    ctx.run(
      mapSystemNote
        .filter(_.id == lift(id))
        .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
    )

  def deleteMapSystemStructure(
      mapId: MapId,
      systemId: SystemId,
      name: String,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      mapSystemStructure
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .filter(_.name == lift(name))
        .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
    )

  def hardDeleteMapWormholeSignatures(mapId: MapId, sigs: Chunk[MapSystemSignature]): DbOperation[Long] =
    ctx
      .run(
        quote(
          liftQuery(sigs).foreach(mss =>
            mapSystemSignature
              .filter(_.mapId == lift(mapId))
              .filter(_.systemId == mss.systemId)
              .filter(_.signatureId == mss.signatureId)
              .delete
          )
        )
      )
      .map(_.sum)

  def hardDeleteMapWormholeConnections(mapId: MapId, connectionIds: Chunk[ConnectionId]): DbOperation[Long] =
    ctx.run(quote(liftQuery(connectionIds).foreach(cId => mapWormholeConnection.filter(_.id == cId).delete))).map(_.sum)

  def hardDeleteMapWormholeConnectionJumps(mapId: MapId, connectionIds: Chunk[ConnectionId]): DbOperation[Long] =
    ctx
      .run(
        quote(liftQuery(connectionIds).foreach(cId => mapWormholeConnectionJump.filter(_.connectionId == cId).delete))
      )
      .map(_.sum)

  def vacuumMap: DbOperation[Long] =
    // TODO this is a bit strange
    inline val vac = "VACUUM map;"
    ctx.run(quote(infix"#$vac".as[Action[Unit]]))

private def whClassesFromSet(set: BitSet): Set[WormholeClass] =
  val res = Set.newBuilder[WormholeClass]
  set.foreach(i => WormholeClasses.ById.get(i).foreach(res.addOne))
  res.result()
