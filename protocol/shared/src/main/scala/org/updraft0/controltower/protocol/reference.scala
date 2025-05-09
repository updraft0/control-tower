package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.{WormholeClass, TypeId}

/** Pre-loaded reference of types that are commonly needed in the web app
  */
case class Reference(
    version: Int,
    factions: Array[Faction],
    signaturesInGroup: Array[SignatureInGroup],
    shipTypes: Array[ShipType],
    starTypes: Array[StarType],
    stationOperations: Array[StationOperation],
    wormholeTypes: Array[WormholeType],
    structureTypes: Array[StructureType]
)

/** Version of (data, code) - if either changes, reference should be reloaded
  */
case class ReferenceVersion(data: Int, code: String)

case class ReferenceSolarSystems(version: Int, solarSystems: Array[SolarSystem])

case class StationService(id: Int, name: String)

/** All station operations with services
  */
case class StationOperation(operationId: Int, operationName: String, services: Array[StationService])

/** Wormhole type
  */
case class WormholeType(
    typeId: TypeId,
    name: String,
    massRegeneration: Long,
    maxJumpMass: Long,
    maxStableMass: Long,
    maxStableTime: Long,
    targetClass: WormholeClass
):
  def massSize: WormholeMassSize =
    if maxJumpMass >= 1_000_000_000 then WormholeMassSize.XL
    else if maxJumpMass >= 375_000_000 then WormholeMassSize.L
    else if maxJumpMass >= 62_000_000 then WormholeMassSize.M
    else WormholeMassSize.S

  def stablePoints: Double = (maxStableMass / 10_000_000).toDouble / 10.0d
  def jumpPoints: Double   = (maxJumpMass / 10_000_000).toDouble / 10.0d

/** Signature group/name/wormhole class
  */
case class SignatureInGroup(signatureGroup: SignatureGroup, name: String, targetClasses: Array[WormholeClass])

/** Star/sun type ids
  */
case class StarType(typeId: TypeId, name: String)

case class Faction(id: Long, name: String, corporationId: Option[Long], militiaCorporationId: Option[Long])

case class ShipType(typeId: TypeId, name: String, groupId: Long, groupName: String, mass: Long)
