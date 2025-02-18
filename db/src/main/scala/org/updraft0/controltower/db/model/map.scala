package org.updraft0.controltower.db.model

import org.updraft0.controltower.constant.*

import java.time.Instant
import scala.CanEqual

enum ChainNamingStrategy extends Enum[ChainNamingStrategy] derives CanEqual:
  case Manual

enum SignatureGroup extends Enum[SignatureGroup] derives CanEqual:
  case Unknown, Combat, Data, Gas, Ghost, Ore, Relic, Wormhole

enum WormholeMassSize extends Enum[WormholeMassSize] derives CanEqual:
  case Unknown, XL, L, M, S

enum WormholeMassStatus extends Enum[WormholeMassStatus] derives CanEqual:
  case Unknown, Fresh, Reduced, Critical

enum WormholeK162Type extends Enum[WormholeK162Type] derives CanEqual:
  case Unknown, Dangerous, Deadly, Hisec, Losec, Nullsec, Thera

enum IntelStance extends Enum[IntelStance] derives CanEqual:
  case Unknown, Friendly, Hostile

enum IntelGroup extends Enum[IntelGroup] derives CanEqual:
  case Unknown, HQ, Farm, Staging

enum MapDisplayType extends Enum[MapDisplayType] derives CanEqual:
  case Manual

enum SystemDisplayData:
  /** Manual position of system on grid in (x, y) coordinate (not pixels)
    */
  case Manual(x: Int, y: Int)

extension (sd: SystemDisplayData)
  def displayType: MapDisplayType = sd match
    case _: SystemDisplayData.Manual => MapDisplayType.Manual

// note: using the name `Map` conflicts with prelude
case class MapModel(
    id: MapId,
    name: String,
    displayType: MapDisplayType,
    createdAt: Instant,
    creatorUserId: UserId,
    deletedAt: Option[Instant],
    deletedByUserId: Option[UserId]
)

case class MapSystem(
    mapId: MapId,
    systemId: SystemId,
    name: Option[String],
    isPinned: Boolean,
    chainNamingStrategy: ChainNamingStrategy,
    description: Option[String],
    stance: IntelStance,
    updatedByCharacterId: CharacterId,
    updatedAt: Instant
)

case class MapSystemDisplay(mapId: MapId, systemId: SystemId, displayType: MapDisplayType, data: SystemDisplayData)

case class MapWormholeConnection(
    id: ConnectionId,
    mapId: MapId,
    fromSystemId: SystemId,
    toSystemId: SystemId,
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapWormholeConnectionJump(
    connectionId: ConnectionId,
    characterId: CharacterId,
    shipTypeId: TypeId,
    massOverride: Option[Long],
    createdAt: Instant
)

case class MapSystemSignature(
    mapId: MapId,
    systemId: SystemId,
    signatureId: SigId,
    isDeleted: Boolean,
    signatureGroup: SignatureGroup,
    signatureTypeName: Option[String],
    wormholeIsEol: Option[Boolean],
    wormholeEolAt: Option[Instant],
    wormholeTypeId: Option[TypeId],
    wormholeMassSize: Option[WormholeMassSize],
    wormholeMassStatus: Option[WormholeMassStatus],
    wormholeK162Type: Option[WormholeK162Type],
    wormholeConnectionId: Option[ConnectionId],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
) derives CanEqual

case class Alliance(
    id: AllianceId,
    name: String,
    ticker: String,
    creatorCharacterId: CharacterId,
    creatorCorporationId: CorporationId,
    executorCorporationId: Option[CorporationId],
    createdAt: Instant,
    updatedAt: Instant
)

case class Corporation(
    id: CorporationId,
    name: String,
    ticker: String,
    allianceId: Option[AllianceId],
    ceoCharacterId: CharacterId,
    creatorCharacterId: CharacterId,
    homeStationId: Long,
    memberCount: Int,
    url: Option[String],
    createdAt: Instant,
    updatedAt: Instant
)

case class IntelSystemStructure(
    id: IntelStructureId,
    mapId: MapId,
    systemId: SystemId,
    name: Option[String],
    ownerCorporationId: Option[CorporationId],
    itemTypeId: TypeId,
    nearestPlanetIdx: Option[Int],
    nearestMoonIdx: Option[Int],
    isOnline: Option[Boolean],
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId,
    deletedAt: Option[Instant],
    deletedByCharacterId: Option[CharacterId]
)

case class IntelSystem(
    mapId: MapId,
    systemId: SystemId,
    primaryCorporationId: Option[CorporationId],
    primaryAllianceId: Option[AllianceId],
    intelGroup: IntelGroup,
    isEmpty: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class IntelSystemNote(
    id: IntelNoteId,
    mapId: MapId,
    systemId: SystemId,
    note: String,
    isPinned: Boolean,
    isDeleted: Boolean,
    originalId: Option[IntelNoteId],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    deletedAt: Option[Instant],
    deletedByCharacterId: Option[CharacterId]
)

case class IntelSystemPing(
    id: IntelPingId,
    mapId: MapId,
    systemId: SystemId,
    pingUserId: Option[UserId],
    pingMapGlobal: Option[Boolean],
    pingNote: Option[String],
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    deletedAt: Option[Instant],
    deletedByCharacterId: Option[CharacterId]
)

case class IntelGroupStance(
    mapId: MapId,
    corporationId: Option[CorporationId],
    allianceId: Option[AllianceId],
    stance: IntelStance,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class IntelCharacter(
    id: CharacterId,
    bloodlineId: Int,
    corporationId: CorporationId,
    factionId: Option[Int],
    gender: String,
    name: String,
    raceId: Int,
    securityStatus: Option[Float],
    title: Option[String],
    createdAt: Instant,
    updatedAt: Instant
)
