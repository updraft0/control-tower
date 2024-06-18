package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant
import org.updraft0.controltower.constant.{SystemId => _, *}
import java.time.{Instant, Duration}

type SystemId = Long // TODO opaque types

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

case class Corporation(id: CorporationId, name: String)

case class MapSystemStructure(
    name: String,
    structureType: Option[String], /* FIXME */
    owner: Option[Corporation],
    location: Option[String],
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapSystemNote(
    id: Long,
    note: String,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
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
    shipTypeId: Int,
    massOverride: Option[Int],
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

enum IntelGroup derives CanEqual:
  case Unknown, HQ, Farm, Staging

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
    shipTypeId: Long,
    shipName: String,
    structureId: Option[Long],
    stationId: Option[Int],
    updatedAt: Instant
)

enum WormholeConnectionType derives CanEqual:
  case Unknown
  case K162(sub: WormholeK162Type)
  case Known(typeId: Long)

enum MapSystemSignature(
    val id: SigId,
    val createdAt: Instant,
    val createdByCharacterId: CharacterId,
    val updatedAt: Instant,
    val updatedByCharacterId: CharacterId,
    val signatureGroup: SignatureGroup
) derives CanEqual:
  case Unknown(
      override val id: SigId,
      override val createdAt: Instant,
      override val createdByCharacterId: CharacterId,
      override val updatedAt: Instant,
      override val updatedByCharacterId: CharacterId
  ) extends MapSystemSignature(
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        SignatureGroup.Unknown
      )
  case Wormhole(
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
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        SignatureGroup.Wormhole
      )

  case Site(
      override val id: SigId,
      group: SignatureGroup,
      name: Option[String],
      override val createdAt: Instant,
      override val createdByCharacterId: CharacterId,
      override val updatedAt: Instant,
      override val updatedByCharacterId: CharacterId
  ) extends MapSystemSignature(
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
      connectionId: Option[ConnectionId]
  ) extends NewSystemSignature(id, createdAt)

  def signatureGroup: SignatureGroup = this match
    case _: Unknown  => SignatureGroup.Unknown
    case _: Wormhole => SignatureGroup.Wormhole
    case s: Site     => s.group

case class MapSystemSnapshot(
    system: MapSystem,
    display: Option[SystemDisplayData],
    signatures: Array[MapSystemSignature],
    notes: Array[MapSystemNote],
    structures: Array[MapSystemStructure],
    connections: Array[MapWormholeConnection]
) derives CanEqual

enum MapServerStatus derives CanEqual:
  case Error
  case Online(players: Int, version: String, startedAt: String, vip: Boolean)

enum NewSystemName derives CanEqual:
  case None
  case Name(value: String)

  def toOption: Option[String] = this match
    case None        => Option.empty
    case Name(value) => Some(value)

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
      name: Option[NewSystemName],
      isPinned: Boolean,
      displayData: SystemDisplayData,
      stance: Option[IntelStance]
  )

  /** (idempotent) Add/update a system scan signature
    */
  case AddSystemSignature(systemId: Long, sig: NewSystemSignature)

  /** (non-idempotent) Add a connection between two systems
    */
  case AddSystemConnection(fromSystemId: constant.SystemId, toSystemId: constant.SystemId)

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
      name: Option[NewSystemName] = None // TODO: weirdly cannot nest Option[Option[_]]
  )

  /** Remove a system from the map (this only deletes display data and clears the pinned status)
    */
  case RemoveSystem(systemId: SystemId)

  /** Remove a connection from the map
    */
  case RemoveSystemConnection(connectionId: ConnectionId)

// TODO: have a think about connections and when they get cleaned up (delete connection *and* delete signature?)
// TODO: enforce the level of permissions that a user can have! (and document that)
// TODO: move to opaque types

enum MapMessage:
  case CharacterLocations(locations: Map[constant.SystemId, Array[CharacterLocation]])
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
  case SystemRemoved(
      removedSystem: MapSystemSnapshot,
      removedConnectionIds: Array[ConnectionId],
      connections: Map[ConnectionId, MapWormholeConnectionWithSigs]
  )
  case ServerStatus(status: MapServerStatus)

// endregion
