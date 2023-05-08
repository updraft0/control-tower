package org.updraft0.controltower.protocol
import org.updraft0.controltower.constant.WormholeClass

case class StationService(id: Int, name: String)

/** All station operations with services
  */
case class StationOperationReference(operationId: Int, operationName: String, services: Vector[StationService])

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

case class StarType(typeId: Long, name: String)

// TODO: faction
// TODO: star
// TODO: wormhole mass information
// TODO: ship mass
