package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.WormholeClass

/** Pre-loaded reference of types that are commonly needed in the web app
  */
case class Reference(
    version: Int,
    factions: List[Faction],
    shipTypes: List[ShipType],
    starTypes: List[StarType],
    stationOperations: List[StationOperation],
    wormholeTypes: List[WormholeType]
)

case class ReferenceSolarSystems(version: Int, solarSystems: List[SolarSystem])

case class StationService(id: Int, name: String)

/** All station operations with services
  */
case class StationOperation(operationId: Int, operationName: String, services: Vector[StationService])

/** Wormhole type
  */
case class WormholeType(
    typeId: Long,
    name: String,
    massRegeneration: Long,
    maxJumpMass: Long,
    maxStableMass: Long,
    maxStableTime: Long,
    targetClass: WormholeClass
)

/** Star/sun type ids
  */
case class StarType(typeId: Long, name: String)

case class Faction(id: Long, name: String, corporationId: Option[Long], militiaCorporationId: Option[Long])

case class ShipType(typeId: Long, name: String, groupId: Long, groupName: String, mass: Long)
