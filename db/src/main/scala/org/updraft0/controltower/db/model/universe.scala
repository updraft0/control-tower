package org.updraft0.controltower.db.model

import org.updraft0.controltower.constant.{SystemId, WormholeClass}

import java.time.Instant

case class Constellation(id: Long, name: String, regionId: Long, regionName: String)
case class Region(id: Long, name: String, whClassId: Option[Int], factionId: Option[Long])

case class NpcStation(
    id: Long,
    name: String,
    ownerId: Long,
    typeId: Long,
    operationId: Long,
    planetId: Long,
    moonId: Option[Long],
    systemId: SystemId
)
case class SolarSystemAsteroidBelt(id: Long, planetId: Long, systemId: SystemId)
case class SolarSystemPlanet(id: Long, systemId: SystemId, idx: Long, typeId: Long)
case class SolarSystemMoon(id: Long, planetId: Long, systemId: SystemId, idx: Long)
case class SolarSystemStar(id: Long, type_id: Long)
case class SolarSystem(
    id: SystemId,
    starId: Option[Long],
    starTypeId: Option[Long],
    name: String,
    regionName: String,
    regionId: Long,
    constellationName: String,
    constellationId: Long,
    effectTypeId: Option[Long],
    whClassId: Option[Int],
    securityClass: Option[String],
    security: Option[Double],
    border: Boolean,
    corridor: Boolean,
    fringe: Boolean,
    hub: Boolean,
    international: Boolean,
    regional: Boolean
)

case class Stargate(id: Long, systemId: SystemId, toStargateId: Long)

// from map

case class SystemStaticWormhole(
    systemId: SystemId,
    staticTypeId: Long,
    validFrom: Option[Instant] = None,
    validUntil: Option[Instant] = None,
    updatedAt: Option[Instant] = None
)

case class Wormhole(
    typeId: Long,
    name: String,
    massRegeneration: Long,
    maxJumpMass: Long,
    maxStableMass: Long,
    maxStableTime: Long,
    targetClass: WormholeClass
)

case class SignatureInGroup(signatureGroup: SignatureGroup, name: String, targetClasses: Set[WormholeClass])
