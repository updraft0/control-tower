package org.updraft0.controltower.db.model

import java.time.Instant
import scala.CanEqual

type MapId    = Long
type SystemId = Long

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
case class MapModel(id: MapId, name: String, displayType: MapDisplayType, createdAt: Instant, creatorUserId: UserId)

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

case class MapSystemStructure(
    mapId: MapId,
    systemId: SystemId,
    name: String,
    isDeleted: Boolean,
    ownerCorporationId: Option[CorporationId],
    structureType: Option[String], // TODO improve categorisation
    location: Option[String],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapSystemNote(
    id: Long,
    mapId: MapId,
    systemId: SystemId,
    note: String,
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapWormholeConnection(
    id: Long,
    mapId: MapId,
    fromSystemId: SystemId,
    toSystemId: SystemId,
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapSystemSignature(
    mapId: MapId,
    systemId: SystemId,
    signatureId: String,
    isDeleted: Boolean,
    signatureGroup: SignatureGroup,
    signatureTypeName: Option[String],
    wormholeIsEol: Option[Boolean],
    wormholeEolAt: Option[Instant],
    wormholeTypeId: Option[Long],
    wormholeMassSize: Option[WormholeMassSize],
    wormholeMassStatus: Option[WormholeMassStatus],
    wormholeK162Type: Option[WormholeK162Type],
    wormholeConnectionId: Option[Long],
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
    executorCorporationId: CorporationId,
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
