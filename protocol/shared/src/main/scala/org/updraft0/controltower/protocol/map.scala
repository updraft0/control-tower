package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
import java.time.{Instant, Duration}

enum PolicyMemberType derives CanEqual:
  case Character, Corporation, Alliance

case class MapPolicyMember(
    memberId: Long,
    memberType: PolicyMemberType,
    isDeny: Boolean,
    role: MapRole,
    createdBy: Option[Long] = None,
    createdAt: Option[Instant] = None,
    updatedBy: Option[Long] = None,
    updatedAt: Option[Instant] = None
)

case class MapSettings(
    staleScanThreshold: Duration
)

case class NewMap(name: String, policyMembers: Array[MapPolicyMember], displayType: MapDisplayType)

case class MapInfo(id: Long, name: String, displayType: MapDisplayType, settings: MapSettings, createdAt: Instant)

// region WebSocket protocol

case class MapSystem(
    systemId: Long,
    name: Option[String],
    isPinned: Boolean,
    chainNamingStrategy: Option[Int] /* FIXME */,
    description: Option[String],
    stance: IntelStance,
    display: Option[SystemDisplayData],
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class Corporation(id: Long, name: String)

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
    id: Long,
    fromSystemId: Long,
    toSystemId: Long,
    createdAt: Instant,
    createdByCharacterId: CharacterId,
    updatedAt: Instant,
    updatedByCharacterId: CharacterId
)

case class MapWormholeConnectionRank(fromSystemIdx: Int, fromSystemCount: Int, toSystemIdx: Int, toSystemCount: Int)

case class MapWormholeConnectionWithSigs(
    connection: MapWormholeConnection,
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

enum WormholeConnectionType derives CanEqual:
  case Unknown
  case K162(sub: WormholeK162Type)
  case Known(typeId: Long)

enum MapSystemSignature(
    val id: String,
    val createdAt: Instant,
    val createdByCharacterId: CharacterId,
    val updatedAt: Instant,
    val updatedByCharacterId: CharacterId,
    val signatureGroup: SignatureGroup
) derives CanEqual:
  case Unknown(
      override val id: String,
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
      override val id: String,
      eolAt: Option[Instant],
      connectionType: WormholeConnectionType,
      massStatus: WormholeMassStatus,
      massSize: WormholeMassSize,
      connectionId: Option[Long],
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
      override val id: String,
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
      connectionId: Option[Long]
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
      systemId: Long,
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
  case AddSystemConnection(fromSystemId: SystemId, toSystemId: SystemId)

  /** (idempotent) Update multiple system signatures
    */
  case UpdateSystemSignatures(systemId: Long, replaceAll: Boolean, scanned: Array[NewSystemSignature])

  /** (idempotent) Remove selected signatures in system
    */
  case RemoveSystemSignatures(systemId: Long, sigIds: List[SigId])

  /** (idempotent) Remove all signatures in system
    */
  case RemoveAllSystemSignatures(systemId: Long)

  /** Change an aspect of a system (or multiple at the same time)
    */
  case UpdateSystem(
      systemId: Long,
      displayData: Option[SystemDisplayData] = None,
      isPinned: Option[Boolean] = None,
      stance: Option[IntelStance] = None,
      name: Option[NewSystemName] = None // TODO: weirdly cannot nest Option[Option[_]]
  )

  /** Remove a system from the map (this only deletes display data and clears the pinned status)
    */
  case RemoveSystem(systemId: Long)

  /** Remove a connection from the map
    */
  case RemoveSystemConnection(connectionId: Long)

// TODO: have a think about connections and when they get cleaned up (delete connection *and* delete signature?)
// TODO: enforce the level of permissions that a user can have! (and document that)
// TODO: move to opaque types

enum MapMessage:
  case ConnectionSnapshot(connection: MapWormholeConnectionWithSigs)
  case ConnectionsRemoved(connections: Array[MapWormholeConnection])
  case Error(message: String)
  case MapSnapshot(systems: Map[Long, MapSystemSnapshot], connections: Map[Long, MapWormholeConnectionWithSigs])
  case MapMeta(characterId: CharacterId, info: MapInfo, role: MapRole)
  case SystemSnapshot(systemId: Long, system: MapSystemSnapshot, connections: Map[Long, MapWormholeConnectionWithSigs])
  case SystemDisplayUpdate(systemId: Long, name: Option[String], displayData: SystemDisplayData)
  case SystemRemoved(systemId: Long)

// endregion
