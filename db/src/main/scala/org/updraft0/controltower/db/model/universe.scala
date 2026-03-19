package org.updraft0.controltower.db.model

import org.updraft0.controltower.constant.{TypeId, SystemId, WormholeClass}

import java.time.Instant

case class Constellation(id: Long, name: String, regionId: Long, regionName: String)
case class Region(id: Long, name: String, whClassId: Option[Int], factionId: Option[Long])

case class NpcStation(
    id: Long,
    name: String,
    ownerId: Long,
    typeId: Long,
    operationId: Long,
    orbitId: Long,
    orbitIndex: Option[Int],
    celestialIndex: Option[Int],
    systemId: SystemId
)
case class SolarSystemEffect(id: Long, systemId: SystemId, typeId: TypeId)
case class SolarSystemPlanet(id: Long, systemId: SystemId, idx: Long, typeId: TypeId, moonCount: Int)
case class SolarSystemStar(id: Long, systemId: SystemId, typeId: TypeId)
case class SolarSystem(
    id: SystemId,
    starId: Option[Long],
    name: String,
    regionName: String,
    regionId: Long,
    constellationName: String,
    constellationId: Long,
    effectTypeId: Option[Int],
    whClassId: Option[Int],
    security: Option[Double]
)

case class Stargate(id: Long, systemId: SystemId, toStargateId: Long, toSystemId: SystemId)

// from map

case class SystemStaticWormhole(
    systemId: SystemId,
    staticTypeId: TypeId,
    validFrom: Option[Instant] = None,
    validUntil: Option[Instant] = None,
    updatedAt: Option[Instant] = None
)

case class Wormhole(
    typeId: TypeId,
    name: String,
    massRegeneration: Long,
    maxJumpMass: Long,
    maxStableMass: Long,
    maxStableTime: Long,
    targetClass: WormholeClass
)

case class SignatureInGroup(signatureGroup: SignatureGroup, name: String, targetClasses: Set[WormholeClass])
