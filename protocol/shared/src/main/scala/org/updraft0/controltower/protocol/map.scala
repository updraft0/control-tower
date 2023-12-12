package org.updraft0.controltower.protocol

import java.time.Instant
import scala.language.implicitConversions

enum PolicyMemberType:
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

case class NewMap(name: String, policyMembers: Vector[MapPolicyMember], displayType: MapDisplayType)

case class MapInfo(id: Long, name: String, displayType: MapDisplayType, createdAt: Instant)

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
    updatedByCharacterId: Long
)

case class Corporation(id: Long, name: String)

case class MapSystemStructure(
    name: String,
    structureType: Option[String], /* FIXME */
    owner: Option[Corporation],
    location: Option[String],
    createdAt: Instant,
    createdByCharacterId: Long,
    updatedAt: Instant,
    updatedByCharacterId: Long
)

case class MapSystemNote(
    id: Long,
    note: String,
    createdAt: Instant,
    createdByCharacterId: Long,
    updatedAt: Instant,
    updatedByCharacterId: Long
)

case class MapWormholeConnection(
    id: Long,
    fromSystemId: Long,
    toSystemId: Long,
    createdAt: Instant,
    createdByCharacterId: Long,
    updatedAt: Instant,
    updatedByCharacterId: Long
)

enum SignatureGroup:
  case Unknown, Combat, Data, Gas, Ghost, Ore, Relic, Wormhole

enum WormholeMassSize:
  case Unknown, XL, L, M, S

enum WormholeMassStatus:
  case Unknown, Fresh, Reduced, Critical

enum WormholeK162Type:
  case Unknown, Dangerous, Deadly

enum IntelStance:
  case Unknown, Friendly, Hostile

enum MapDisplayType:
  case Manual

enum SystemDisplayData:
  /** Manual position of system on grid in (x, y) coordinate (not pixels)
    */
  case Manual(x: Int, y: Int)

extension (sd: SystemDisplayData)
  def displayType: MapDisplayType = sd match
    case _: SystemDisplayData.Manual => MapDisplayType.Manual

enum WormholeConnectionType:
  case Unknown
  case K162(sub: WormholeK162Type)
  case Known(typeId: Long)

enum MapSystemSignature(
    val id: String,
    val createdAt: Instant,
    val createdByCharacterId: Long,
    val updatedAt: Instant,
    val updatedByCharacterId: Long,
    val signatureGroup: SignatureGroup
):
  case Unknown(
      override val id: String,
      override val createdAt: Instant,
      override val createdByCharacterId: Long,
      override val updatedAt: Instant,
      override val updatedByCharacterId: Long
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
      isEol: Option[Boolean],
      eolAt: Option[Instant],
      connectionType: WormholeConnectionType,
      massStatus: WormholeMassStatus,
      massSize: WormholeMassSize,
      connectionId: Option[Long],
      override val createdAt: Instant,
      override val createdByCharacterId: Long,
      override val updatedAt: Instant,
      override val updatedByCharacterId: Long
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
      override val createdByCharacterId: Long,
      override val updatedAt: Instant,
      override val updatedByCharacterId: Long
  ) extends MapSystemSignature(
        id,
        createdAt,
        createdByCharacterId,
        updatedAt,
        updatedByCharacterId,
        group
      )

case class MapSystemSnapshot(
    system: MapSystem,
    display: Option[SystemDisplayData],
    signatures: Vector[MapSystemSignature],
    notes: Vector[MapSystemNote],
    structures: Vector[MapSystemStructure],
    connections: Vector[MapWormholeConnection]
)

enum NewSystemName:
  case None
  case Name(value: String)

  def toOption: Option[String] = this match
    case None        => Option.empty
    case Name(value) => Some(value)

enum MapRequest:
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

// TODO: have a think about connections and when they get cleaned up (delete connection *and* delete signature?)
// TODO: enforce the level of permissions that a user can have! (and document that)

enum MapMessage:
  case MapSnapshot(systems: Vector[MapSystemSnapshot], connections: Map[Long, MapWormholeConnection])
  case MapMeta(info: MapInfo, role: MapRole)
  case SystemSnapshot(systemId: Long, system: MapSystemSnapshot, connections: Map[Long, MapWormholeConnection])
  case SystemDisplayUpdate(systemId: Long, name: Option[String], displayData: SystemDisplayData)
  case SystemRemoved(systemId: Long)

// endregion
