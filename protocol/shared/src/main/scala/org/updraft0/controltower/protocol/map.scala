package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*

import java.time.{Duration, Instant}

enum PolicyMemberType derives CanEqual:
  case Character, Corporation, Alliance

case class MapPolicyMember(
    memberId: Long,
    memberType: PolicyMemberType,
    isDeny: Boolean,
    role: MapRole,
    createdBy: Option[UserId] = None,
    createdAt: Option[Instant] = None,
    updatedBy: Option[UserId] = None,
    updatedAt: Option[Instant] = None
) derives CanEqual

case class MapSettings(
    staleScanThreshold: Duration
)

case class NewMap(name: String, policyMembers: Array[MapPolicyMember], displayType: MapDisplayType)

case class MapInfo(id: MapId, name: String, displayType: MapDisplayType, settings: MapSettings, createdAt: Instant)

case class MapInfoWithPermissions(map: MapInfo, policyMembers: Array[MapPolicyMember])

// region Intel

enum UpwellStructureSize derives CanEqual:
  case Medium, Large, XL

enum UpwellStructureType derives CanEqual:
  case Astrahus, Fortizar, Keepstar, Raitaru, Azbel, Sotiyo, Athanor, Tatara

  def size: UpwellStructureSize =
    this match
      case Astrahus | Raitaru | Athanor => UpwellStructureSize.Medium
      case Fortizar | Azbel | Tatara    => UpwellStructureSize.Large
      case Keepstar | Sotiyo            => UpwellStructureSize.XL

enum PlayerStructureSize derives CanEqual:
  case Small, Medium, Large

enum StructureType derives CanEqual:
  case Upwell(`type`: UpwellStructureType, typeName: String, typeId: TypeId)
  case PlayerOwned(size: PlayerStructureSize, typeName: String, typeId: TypeId)

  def typeAndName: (TypeId, String) =
    this match
      case u: Upwell      => (u.typeId, u.typeName)
      case p: PlayerOwned => (p.typeId, p.typeName)

enum IntelGroup derives CanEqual:
  case Unknown, HQ, Farm, Staging

case class IntelSystemStructure(
    id: IntelStructureId,
    systemId: SystemId,
    `type`: StructureType,
    name: Option[String],
    ownerCorporation: Option[Corporation],
    nearestPlanetIdx: Option[Int], // idx
    nearestMoonIdx: Option[Int],   // idx
    isOnline: Option[Boolean],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class IntelSystem(
    primaryCorporation: Option[Corporation],
    primaryAlliance: Option[Alliance],
    isEmpty: Boolean,
    intelGroup: IntelGroup,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class IntelSystemNote(
    id: IntelNoteId,
    note: String,
    isPinned: Boolean,
    isDeleted: Boolean,
    originalId: Option[IntelNoteId],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    deletedAt: Option[Instant],
    deletedByCharacterId: Option[CharacterId]
)

enum IntelSystemPingTarget derives CanEqual:
  case User, Map

case class IntelSystemPing(
    id: IntelPingId,
    pingTarget: IntelSystemPingTarget,
    pingNote: Option[String],
    isDeleted: Boolean,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    deletedAt: Option[Instant],
    deletedByCharacterId: Option[CharacterId]
)

// endregion

// region WebSocket protocol

case class MapSystem(
    systemId: SystemId,
    name: Option[String],
    isPinned: Boolean,
    chainNamingStrategy: Option[Int] /* FIXME */,
    description: Option[String],
    stance: IntelStance,
    display: Option[SystemDisplayData],
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class Alliance(id: AllianceId, name: String, ticker: String, createdAt: Instant)
case class Corporation(
    id: CorporationId,
    name: String,
    ticker: String,
    alliance: Option[Alliance],
    memberCount: Int,
    createdAt: Instant
)

enum StanceTarget:
  case Alliance(id: AllianceId)
  case Corporation(id: CorporationId)

case class NewIntelSystemStructure(
    `type`: StructureType,
    name: Option[String],
    ownerCorporation: Option[CorporationId],
    nearestPlanetIdx: Option[Int],
    nearestMoonIdx: Option[Int],
    isOnline: Option[Boolean]
)

case class MapWormholeConnection(
    id: ConnectionId,
    fromSystemId: SystemId,
    toSystemId: SystemId,
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

case class MapWormholeConnectionRank(fromSystemIdx: Int, fromSystemCount: Int, toSystemIdx: Int, toSystemCount: Int)

case class MapWormholeConnectionWithSigs(
    connection: MapWormholeConnection,
    jumps: Array[MapWormholeConnectionJump],
    fromSignature: Option[MapSystemSignature.Wormhole],
    toSignature: Option[MapSystemSignature.Wormhole],
    rank: MapWormholeConnectionRank
) derives CanEqual

enum SignatureGroup derives CanEqual:
  case Unknown, Combat, Data, Gas, Ghost, Ore, Relic, Wormhole

enum WormholeMassSize derives CanEqual:
  case Unknown, XL, L, M, S

enum WormholeMassStatus derives CanEqual:
  case Unknown, Fresh, Reduced, Critical

enum WormholeK162Type derives CanEqual:
  case Unknown, Dangerous, Deadly, Hisec, Losec, Nullsec, Thera

  def possibleTarget: List[WormholeClass] = this match
    case Unknown   => List(WormholeClass.C1, WormholeClass.C2, WormholeClass.C3)
    case Dangerous => List(WormholeClass.C4, WormholeClass.C5)
    case Deadly    => List(WormholeClass.C6)
    case Hisec     => List(WormholeClass.H)
    case Losec     => List(WormholeClass.L)
    case Nullsec   => List(WormholeClass.NS)
    case Thera     => List(WormholeClass.Thera)

enum IntelStance derives CanEqual:
  case Unknown, Friendly, Hostile

enum MapDisplayType derives CanEqual:
  case Manual

enum SystemDisplayData:
  /** Manual position of system on grid in (x, y) coordinate (not pixels)
    */
  case Manual(x: Int, y: Int)

extension (sd: SystemDisplayData)
  def displayType: MapDisplayType = sd match
    case _: SystemDisplayData.Manual => MapDisplayType.Manual

case class CharacterLocation(
    characterId: CharacterId,
    characterName: String,
    shipTypeId: TypeId,
    shipName: String,
    structureId: Option[Long],
    stationId: Option[Int],
    updatedAt: Instant
)

enum WormholeConnectionType derives CanEqual:
  case Unknown
  case K162(sub: WormholeK162Type)
  case Known(typeId: TypeId)

enum MapSystemSignature(
    val systemId: SystemId,
    val id: SigId,
    val createdAt: Instant,
    val createdByCharacterId: CharacterId,
    val updatedAt: Instant,
    val updatedByCharacterId: CharacterId,
    val signatureGroup: SignatureGroup
) derives CanEqual:
  case Unknown(
      override val systemId: SystemId,
      override val id: SigId,
      override val createdAt: Instant,
      override val createdByCharacterId: CharacterId,
      override val updatedAt: Instant,
      override val updatedByCharacterId: CharacterId
  ) extends MapSystemSignature(
        systemId,
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        SignatureGroup.Unknown
      )
  case Wormhole(
      override val systemId: SystemId,
      override val id: SigId,
      eolAt: Option[Instant],
      connectionType: WormholeConnectionType,
      massStatus: WormholeMassStatus,
      massSize: WormholeMassSize,
      connectionId: Option[ConnectionId],
      override val createdAt: Instant,
      override val createdByCharacterId: CharacterId,
      override val updatedAt: Instant,
      override val updatedByCharacterId: CharacterId
  ) extends MapSystemSignature(
        systemId,
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        SignatureGroup.Wormhole
      )

  case Site(
      override val systemId: SystemId,
      override val id: SigId,
      group: SignatureGroup,
      name: Option[String],
      override val createdAt: Instant,
      override val createdByCharacterId: CharacterId,
      override val updatedAt: Instant,
      override val updatedByCharacterId: CharacterId
  ) extends MapSystemSignature(
        systemId,
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        group
      )

enum NewSystemSignature(val id: SigId, val createdAt: Instant):
  case Unknown(override val id: SigId, override val createdAt: Instant) extends NewSystemSignature(id, createdAt)
  case Site(
      override val id: SigId,
      override val createdAt: Instant,
      group: SignatureGroup,
      name: Option[String]
  ) extends NewSystemSignature(id, createdAt)
  case Wormhole(
      override val id: SigId,
      override val createdAt: Instant,
      isEol: Boolean,
      connectionType: WormholeConnectionType,
      massStatus: WormholeMassStatus,
      massSize: WormholeMassSize,
      connectionId: UnknownOrUnset[ConnectionId]
  ) extends NewSystemSignature(id, createdAt)

  def signatureGroup: SignatureGroup = this match
    case _: Unknown  => SignatureGroup.Unknown
    case _: Wormhole => SignatureGroup.Wormhole
    case s: Site     => s.group

case class MapSystemSnapshot(
    system: MapSystem,
    display: Option[SystemDisplayData],
    signatures: Array[MapSystemSignature],
    connections: Array[MapWormholeConnection],
    intel: Option[IntelSystem],
    notes: Array[IntelSystemNote],
    structures: Array[IntelSystemStructure],
    pings: Array[IntelSystemPing]
) derives CanEqual

enum NotificationMessage:
  case SystemPing(pingId: IntelPingId, systemId: SystemId, text: Option[String])

case class IntelGroupStance(
    target: StanceTarget,
    stance: IntelStance,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

enum MapServerStatus derives CanEqual:
  case Error
  case Online(players: Int, version: String, startedAt: String, vip: Boolean)

enum MapRequest derives CanEqual:
  /** A snapshot of the map's systems, signatures, connections + intel data
    */
  case GetSnapshot

  /** Get current map information (usually triggered server-side)
    */
  case GetMapInfo

  /** (idempotent) Add/update a system on the map
    */
  case AddSystem(
      systemId: SystemId,
      name: Option[Option[String]],
      isPinned: Boolean,
      displayData: SystemDisplayData,
      stance: Option[IntelStance]
  )

  /** (idempotent) Add/update a system scan signature
    */
  case AddSystemSignature(systemId: SystemId, sig: NewSystemSignature)

  /** (non-idempotent) Add a connection between two systems
    */
  case AddSystemConnection(fromSystemId: SystemId, toSystemId: SystemId)

  /** (idempotent) Add a new system structure
    */
  case AddIntelSystemStructure(systemId: SystemId, structure: NewIntelSystemStructure)

  /** (non-idempotent) Add a new intel system ping
    */
  case AddIntelSystemPing(
      systemId: SystemId,
      pingTarget: IntelSystemPingTarget,
      pingNote: Option[String]
  )

  /** (non-idempotent) Add a new intel system note
    */
  case AddIntelSystemNote(systemId: SystemId, note: String, isPinned: Boolean)

  /** (idempotent) Update multiple system signatures
    */
  case UpdateSystemSignatures(systemId: SystemId, replaceAll: Boolean, scanned: Array[NewSystemSignature])

  /** (idempotent) Remove selected signatures in system
    */
  case RemoveSystemSignatures(systemId: SystemId, sigIds: List[SigId])

  /** (idempotent) Remove all signatures in system
    */
  case RemoveAllSystemSignatures(systemId: SystemId)

  /** Change an aspect of a system (or multiple at the same time)
    */
  case UpdateSystem(
      systemId: SystemId,
      displayData: Option[SystemDisplayData] = None,
      isPinned: Option[Boolean] = None,
      stance: Option[IntelStance] = None,
      name: Option[Option[String]] = None
  )

  /** Update general system intel data
    */
  case UpdateSystemIntel(
      systemId: SystemId,
      primaryCorporation: Option[Option[CorporationId]],
      primaryAlliance: Option[Option[AllianceId]],
      isEmpty: Boolean,
      intelGroup: IntelGroup
  )

  /** Update system intel note
    */
  case UpdateSystemIntelNote(systemId: SystemId, id: IntelNoteId, note: String, isPinned: Boolean)

  /** Update an intel structure in system
    */
  case UpdateIntelSystemStructure(systemId: SystemId, structure: IntelSystemStructure)

  /** Update the stance for an entity
    */
  case UpdateIntelGroupStance(target: StanceTarget, stance: IntelStance)

  /** Remove a system from the map
    */
  case RemoveSystem(systemId: SystemId)

  /** Remove multiple systems from the map
    */
  case RemoveSystems(systemIds: Array[SystemId])

  /** Remove a connection from the map
    */
  case RemoveSystemConnection(connectionId: ConnectionId)

  /** Remove a system structure from intel
    */
  case RemoveIntelSystemStructure(systemId: SystemId, id: IntelStructureId)

  /** Remove a system ping
    */
  case RemoveIntelSystemPing(systemId: SystemId, id: IntelPingId)

  /** Remove a system note
    */
  case RemoveIntelSystemNote(systemId: SystemId, id: IntelNoteId)

end MapRequest

enum MapMessage:
  case CharacterLocations(locations: Map[SystemId, Array[CharacterLocation]])
  case ConnectionSnapshot(connection: MapWormholeConnectionWithSigs)
  case ConnectionsRemoved(connections: Array[MapWormholeConnection])
  case ConnectionJumped(jump: MapWormholeConnectionJump)
  case Error(message: String)
  case MapSnapshot(
      systems: Map[SystemId, MapSystemSnapshot],
      connections: Map[ConnectionId, MapWormholeConnectionWithSigs]
  )
  case MapMeta(character: UserCharacter, info: MapInfo, role: MapRole, preferences: UserPreferences)
  case SystemSnapshot(
      systemId: SystemId,
      system: MapSystemSnapshot,
      connections: Map[ConnectionId, MapWormholeConnectionWithSigs]
  )
  case SystemDisplayUpdate(systemId: SystemId, name: Option[String], displayData: SystemDisplayData)
  case SystemIntelSnapshot(
      systemId: SystemId,
      intel: Option[IntelSystem],
      notes: Array[IntelSystemNote],
      structures: Array[IntelSystemStructure],
      pings: Array[IntelSystemPing]
  )
  case SystemsRemoved(
      removedSystemIds: Array[SystemId],
      removedConnectionIds: Array[ConnectionId],
      updatedSystems: Array[MapSystemSnapshot],
      updatedConnections: Map[ConnectionId, MapWormholeConnectionWithSigs]
  )
  case IntelStanceUpdate(stances: Array[IntelGroupStance])
  case ServerStatus(status: MapServerStatus)
  case Notification(msg: NotificationMessage)

end MapMessage
