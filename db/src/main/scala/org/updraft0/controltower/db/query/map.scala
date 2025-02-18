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

  given MappedEncoding[Int, TypeId] = MappedEncoding(TypeId.apply)
  given MappedEncoding[TypeId, Int] = MappedEncoding(identity)

  given MappedEncoding[Int, IntelNoteId] = MappedEncoding(IntelNoteId.apply)
  given MappedEncoding[IntelNoteId, Int] = MappedEncoding(identity)

  given MappedEncoding[Int, IntelPingId] = MappedEncoding(IntelPingId.apply)
  given MappedEncoding[IntelPingId, Int] = MappedEncoding(identity)

  given MappedEncoding[Int, IntelStructureId] = MappedEncoding(IntelStructureId.apply)
  given MappedEncoding[IntelStructureId, Int] = MappedEncoding(identity)

  // this is always stored as json in the db
  given JsonValueCodec[SystemDisplayData]   = JsonCodecMaker.make(config)
  given JsonValueCodec[Map[String, Double]] = JsonCodecMaker.make(config)

  given MappedEncoding[Int, WormholeClass] = MappedEncoding(WormholeClasses.ById.apply)
  given MappedEncoding[WormholeClass, Int] = MappedEncoding(_.value)

  given MappedEncoding[Int, ChainNamingStrategy] = MappedEncoding(ChainNamingStrategy.fromOrdinal)
  given MappedEncoding[ChainNamingStrategy, Int] = MappedEncoding(_.ordinal())

  given MappedEncoding[Int, IntelGroup] = MappedEncoding(IntelGroup.fromOrdinal)
  given MappedEncoding[IntelGroup, Int] = MappedEncoding(_.ordinal())

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
    inline def mapSystemDisplay      = quote(querySchema[MapSystemDisplay]("map.map_system_display"))
    inline def mapWormholeConnection = quote(querySchema[MapWormholeConnection]("map.map_wormhole_connection"))
    inline def mapWormholeConnectionJump = quote(
      querySchema[MapWormholeConnectionJump]("map.map_wormhole_connection_jump")
    )
    inline def mapSystemSignature = quote(querySchema[MapSystemSignature]("map.map_system_signature"))
    inline def alliance           = quote(querySchema[Alliance]("map.alliance"))
    inline def corporation        = quote(querySchema[Corporation]("map.corporation"))

    inline def intelSystemStructure = quote(querySchema[IntelSystemStructure]("map.intel_system_structure"))
    inline def intelSystem          = quote(querySchema[IntelSystem]("map.intel_system"))
    inline def intelSystemNote      = quote(querySchema[IntelSystemNote]("map.intel_system_note"))
    inline def intelSystemPing      = quote(querySchema[IntelSystemPing]("map.intel_system_ping"))
    inline def intelGroupStance     = quote(querySchema[IntelGroupStance]("map.intel_group_stance"))
    inline def intelCharacter       = quote(querySchema[IntelCharacter]("map.intel_character"))

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

  def getCorporations(ids: Chunk[CorporationId]): DbOperation[Chunk[Corporation]] =
    ctx
      .run(quote(corporation.filter(c => liftQuery(ids).contains(c.id))))
      .map(Chunk.fromIterable)

  def getAlliances(ids: Chunk[AllianceId]): DbOperation[Chunk[Alliance]] =
    ctx
      .run(quote(alliance.filter(a => liftQuery(ids).contains(a.id))))
      .map(Chunk.fromIterable)

  def getIntelCharacters(ids: Chunk[CharacterId]): DbOperation[Chunk[IntelCharacter]] =
    ctx
      .run(quote(intelCharacter.filter(c => liftQuery(ids).contains(c.id))))
      .map(Chunk.fromIterable)

  def getIntelSystem(mapId: MapId, systemId: SystemId): DbOperation[Option[IntelSystem]] =
    ctx
      .run(quote(intelSystem.filter(_.mapId == lift(mapId)).filter(_.systemId == lift(systemId))))
      .map(_.headOption)

  def getIntelSystemNote(mapId: MapId, systemId: SystemId, noteId: IntelNoteId): DbOperation[Option[IntelSystemNote]] =
    ctx
      .run(
        quote(
          intelSystemNote
            .filter(_.id == lift(noteId))
            .filter(_.mapId == lift(mapId))
            .filter(_.systemId == lift(systemId))
        )
      )
      .map(_.headOption)

  def getIntelGroupStanceFor(mapId: MapId): DbOperation[List[IntelGroupStance]] =
    ctx.run(quote(intelGroupStance.filter(_.mapId == lift(mapId))))

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

  def insertIntelSystemStructure(value: IntelSystemStructure): DbOperation[IntelSystemStructure] =
    ctx
      .run(quote(intelSystemStructure.insertValue(lift(value)).returningGenerated(_.id)))
      .map(newId => value.copy(id = newId))

  def insertIntelSystemNote(value: IntelSystemNote): DbOperation[IntelSystemNote] =
    ctx
      .run(quote(intelSystemNote.insertValue(lift(value)).returningGenerated(_.id)))
      .map(newId => value.copy(id = newId))

  def insertIntelSystemPing(value: IntelSystemPing): DbOperation[IntelSystemPing] =
    ctx
      .run(quote(intelSystemPing.insertValue(lift(value)).returningGenerated(_.id)))
      .map(newId => value.copy(id = newId))

  // updates

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

  def updateIntelSystemNote(
      value: IntelSystemNote,
      byCharacterId: CharacterId,
      now: Instant
  ): DbOperation[IntelSystemNote] =
    val copy = value.copy(
      id = IntelNoteId.Invalid,
      originalId = value.originalId.orElse(Some(value.id)),
      createdByCharacterId = byCharacterId,
      createdAt = now,
      deletedByCharacterId = None,
      deletedAt = None
    )
    deleteIntelSystemNote(value.mapId, value.systemId, value.id, byCharacterId) *> insertIntelSystemNote(copy)

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

  def upsertIntelSystemStructure(value: IntelSystemStructure): DbOperation[Long] =
    ctx.run(
      quote {
        intelSystemStructure
          .insertValue(lift(value))
          .onConflictUpdate(_.id)(
            (t, e) => t.name -> e.name,
            (t, e) => t.ownerCorporationId -> e.ownerCorporationId,
            (t, e) => t.itemTypeId -> e.itemTypeId,
            (t, e) => t.nearestPlanetIdx -> e.nearestPlanetIdx,
            (t, e) => t.nearestMoonIdx -> e.nearestMoonIdx,
            (t, e) => t.isOnline -> e.isOnline,
            (t, e) => t.isDeleted -> e.isDeleted,
            (t, e) => t.updatedAt -> e.updatedAt,
            (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId,
            (t, e) => t.deletedAt -> e.deletedAt,
            (t, e) => t.deletedByCharacterId -> e.deletedByCharacterId
          )
      }
    )

  def upsertIntelSystem(value: IntelSystem): DbOperation[Long] =
    ctx.run(
      quote {
        intelSystem
          .insertValue(lift(value))
          .onConflictUpdate(_.mapId, _.systemId)(
            (t, e) => t.primaryCorporationId -> e.primaryCorporationId,
            (t, e) => t.primaryAllianceId -> e.primaryAllianceId,
            (t, e) => t.intelGroup -> e.intelGroup,
            (t, e) => t.isEmpty -> e.isEmpty,
            (t, e) => t.updatedAt -> e.updatedAt,
            (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
          )
      }
    )

  def upsertIntelGroupStance(value: IntelGroupStance): DbOperation[Long] =
    ctx.run(
      quote {
        intelGroupStance
          .insertValue(lift(value))
          .onConflictUpdate(_.mapId, _.corporationId, _.allianceId)(
            (t, e) => t.stance -> e.stance,
            (t, e) => t.updatedAt -> e.updatedAt,
            (t, e) => t.updatedByCharacterId -> e.updatedByCharacterId
          )
      }
    )

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

  def upsertCorporation(value: Corporation): DbOperation[Long] =
    ctx.run(
      quote(
        corporation
          .insertValue(lift(value))
          .onConflictUpdate(_.id)(
            (t, e) => t.allianceId -> e.allianceId,
            (t, e) => t.ceoCharacterId -> e.ceoCharacterId,
            (t, e) => t.homeStationId -> e.homeStationId,
            (t, e) => t.memberCount -> e.memberCount,
            (t, e) => t.ticker -> e.ticker,
            (t, e) => t.url -> e.url,
            (t, e) => t.updatedAt -> unixepoch
          )
      )
    )

  def upsertAlliance(value: Alliance): DbOperation[Long] =
    ctx.run(
      quote(
        alliance
          .insertValue(lift(value))
          .onConflictUpdate(_.id)(
            (t, e) => t.ticker -> e.ticker,
            (t, e) => t.executorCorporationId -> e.executorCorporationId,
            (t, e) => t.updatedAt -> unixepoch
          )
      )
    )

  def upsertIntelCharacter(value: IntelCharacter): DbOperation[Long] =
    ctx.run(
      quote(
        intelCharacter
          .insertValue(lift(value))
          .onConflictUpdate(_.id)(
            (t, e) => t.corporationId -> e.corporationId,
            (t, e) => t.factionId -> e.factionId,
            (t, e) => t.securityStatus -> e.securityStatus,
            (t, e) => t.title -> e.title,
            (t, e) => t.updatedAt -> unixepoch
          )
      )
    )

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

  def deleteIntelSystemNote(
      mapId: MapId,
      systemId: SystemId,
      id: IntelNoteId,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      intelSystemNote
        .filter(_.id == lift(id))
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .update(
          _.isDeleted            -> lift(true),
          _.deletedByCharacterId -> lift(Option(byCharacterId)),
          _.deletedAt            -> unixepochOpt
        )
    )

  def deleteIntelSystemPing(
      mapId: MapId,
      systemId: SystemId,
      id: IntelPingId,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      intelSystemPing
        .filter(_.id == lift(id))
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .update(
          _.isDeleted            -> lift(true),
          _.deletedByCharacterId -> lift(Option(byCharacterId)),
          _.deletedAt            -> unixepochOpt
        )
    )

  def deleteIntelSystemStructure(
      mapId: MapId,
      systemId: SystemId,
      id: IntelStructureId,
      byCharacterId: CharacterId
  ): DbOperation[Long] =
    ctx.run(
      intelSystemStructure
        .filter(_.id == lift(id))
        .filter(_.mapId == lift(mapId))
        .filter(_.systemId == lift(systemId))
        .update(
          _.isDeleted            -> lift(true),
          _.deletedByCharacterId -> lift(Option(byCharacterId)),
          _.deletedAt            -> unixepochOpt
        )
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
