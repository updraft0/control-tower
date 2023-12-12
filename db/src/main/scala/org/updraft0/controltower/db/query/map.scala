package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.*
import zio.ZIO

object map:
  import schema.*
  import ctx.{*, given}
  import zio.json.*

  private val WormholeGroupId = 988L

  // this is always stored as json in the db
  given JsonCodec[SystemDisplayData] = JsonCodec.derived

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

  given MappedEncoding[String, SystemDisplayData] = MappedEncoding(
    _.fromJson[SystemDisplayData].getOrElse(
      throw new RuntimeException("Unable to decode SystemDisplayData from string")
    )
  )
  given MappedEncoding[SystemDisplayData, String] = MappedEncoding(_.toJson)

  /** Each table lives in the `map` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def mapModel              = quote(querySchema[MapModel]("map.map"))
    inline def mapSystem             = quote(querySchema[MapSystem]("map.map_system"))
    inline def mapSystemStructure    = quote(querySchema[MapSystemStructure]("map.map_system_structure"))
    inline def mapSystemNote         = quote(querySchema[MapSystemNote]("map.map_system_note"))
    inline def mapSystemDisplay      = quote(querySchema[MapSystemDisplay]("map.map_system_display"))
    inline def mapWormholeConnection = quote(querySchema[MapWormholeConnection]("map.map_wormhole_connection"))
    inline def mapSystemSignature    = quote(querySchema[MapSystemSignature]("map.map_system_signature"))
    inline def alliance              = quote(querySchema[Alliance]("map.alliance"))
    inline def corporation           = quote(querySchema[Corporation]("map.corporation"))
    inline def systemStaticWormhole  = quote(querySchema[SystemStaticWormhole]("map.ref_system_static_wormhole"))
    inline def wormhole              = quote(querySchema[Wormhole]("map.ref_wormhole"))

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

  def getWormholeSystemNames: DbOperation[Map[String, Long]] =
    ctx.run(sde.schema.solarSystem.filter(_.name like "J%").map(ss => ss.name -> ss.id)).map(_.toMap)

  def getWormholeTypeNames: DbOperation[List[(String, Long)]] =
    ctx.run(sde.schema.itemType.filter(_.groupId == lift(WormholeGroupId)).map(it => it.name -> it.id))

  def getMapSystem(mapId: Long, systemId: Long) =
    ctx
      .run(mapSystem.filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId)))
      .map(_.headOption)

  // upserts
  def upsertMap(value: MapModel): DbOperation[Long] =
    ctx.run(
      mapModel
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.name -> e.name,
          (t, e) => t.displayType -> e.displayType
        )
    )

  def upsertMapSystem(value: MapSystem): DbOperation[Long] =
    ctx.run(
      mapSystem
        .insertValue(lift(value))
        .onConflictUpdate(_.mapId, _.systemId)(
          (t, e) => t.name -> e.name,
          (t, e) => t.isPinned -> e.isPinned,
          (t, e) => t.chainNamingStrategy -> e.chainNamingStrategy,
          (t, e) => t.description -> e.description,
          (t, e) => t.stance -> e.stance,
          (t, _) => t.updatedAt -> unixepoch,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def updateMapSystemName(mapId: Long, systemId: Long, name: Option[String], updatedByCharacterId: Long) =
    ctx.run(
      mapSystem
        .filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId))
        .update(
          _.name                 -> lift(name),
          _.updatedAt            -> unixepoch,
          _.updatedByCharacterId -> lift(updatedByCharacterId)
        )
    )

  def updateMapAttribute(
      mapId: Long,
      systemId: Long,
      isPinned: Option[Boolean],
      intelStance: Option[IntelStance],
      updatedByCharacterId: Long
  ): DbOperation[Long] =
    ctx.run(
      mapSystem
        .filter(ms => ms.mapId == lift(mapId) && ms.systemId == lift(systemId))
        .update(
          ms => ms.stance -> lift(intelStance).getOrElse(ms.stance),
          ms => ms.isPinned -> lift(isPinned).getOrElse(ms.isPinned),
          _.updatedAt            -> unixepoch,
          _.updatedByCharacterId -> lift(updatedByCharacterId)
        )
    )

  def upsertMapSystemDisplay(value: MapSystemDisplay): DbOperation[Long] =
    ctx.run(
      mapSystemDisplay
        .insertValue(lift(value))
        .onConflictUpdate(_.mapId, _.systemId)(
          (t, e) => t.displayType -> e.displayType,
          (t, e) => t.data -> e.data
        )
    )

  def upsertMapSystemStructure(value: MapSystemStructure): DbOperation[Long] =
    ctx.run(
      mapSystemStructure
        .insertValue(lift(value))
        .onConflictUpdate(_.mapId, _.systemId, _.name)(
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, e) => t.ownerCorporationId -> e.ownerCorporationId,
          (t, e) => t.structureType -> e.structureType,
          (t, e) => t.location -> e.location,
          (t, _) => t.updatedAt -> unixepoch,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def upsertMapSystemNote(value: MapSystemNote): DbOperation[Long] =
    ctx.run(
      mapSystemNote
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.note -> e.note,
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, _) => t.updatedAt -> unixepoch,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def upsertMapWormholeConnection(value: MapWormholeConnection): DbOperation[Long] =
    ctx.run(
      mapWormholeConnection
        .insertValue(lift(value))
        .onConflictUpdate(_.id)(
          (t, e) => t.isDeleted -> e.isDeleted,
          (t, _) => t.updatedAt -> unixepoch,
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
          (t, e) => t.wormholeIsEol -> e.wormholeIsEol,
          (t, e) => t.wormholeEolAt -> e.wormholeEolAt,
          (t, e) => t.wormholeTypeId -> e.wormholeTypeId,
          (t, e) => t.wormholeMassSize -> e.wormholeMassSize,
          (t, e) => t.wormholeMassStatus -> e.wormholeMassStatus,
          (t, e) => t.wormholeK162Type -> e.wormholeK162Type,
          (t, e) => t.wormholeConnectionId -> e.wormholeConnectionId,
          (t, _) => t.updatedAt -> unixepoch,
          (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
        )
    )

  def upsertSystemStatic(value: SystemStaticWormhole): DbOperation[Long] =
    ctx.run(systemStaticWormhole.insertValue(lift(value)).onConflictIgnore(_.systemId, _.staticTypeId, _.validFrom))

  def upsertWormhole(value: Wormhole): DbOperation[Long] =
    ctx.run(wormhole.insertValue(lift(value)).onConflictIgnore(_.name))

  // deletes
  def deleteMapSystemDisplay(mapId: MapId, systemId: SystemId): DbOperation[Long] =
    ctx.run(
      mapSystemDisplay
        .filter(msd => msd.mapId == lift(mapId) && msd.systemId == lift(systemId))
        .delete
    )

  def deleteMapWormholeConnection(id: Long, byCharacterId: CharacterId): DbOperation[Long] =
    ctx.run(
      mapWormholeConnection
        .filter(_.id == lift(id))
        .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
    )

  def deleteMapSystemSignature(
      mapId: MapId,
      systemId: SystemId,
      signatureId: String,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      mapSystemSignature
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .filter(_.signatureId == lift(signatureId))
        .update(_.isDeleted -> lift(true), _.updatedByCharacterId -> lift(byCharacterId), _.updatedAt -> unixepoch)
    )

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
