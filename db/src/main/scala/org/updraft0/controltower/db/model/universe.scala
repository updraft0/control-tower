package org.updraft0.controltower.db.model

case class Constellation(id: Long, name: String, regionId: Long, regionName: String)
case class Region(id: Long, name: String, whClassId: Option[Int], factionId: Option[Long])

case class NpcStation(
    id: Long,
    name: String,
    ownerId: Long,
    typeId: Long,
    operationId: Long,
    moonId: Long,
    systemId: Long
)
case class SolarSystemAsteroidBelt(id: Long, planetId: Long, systemId: Long)
case class SolarSystemPlanet(id: Long, systemId: Long, idx: Long, typeId: Long)
case class SolarSystemMoon(id: Long, planetId: Long, systemId: Long, idx: Long)
case class SolarSystemStar(id: Long, type_id: Long)
case class SolarSystem(
    id: Long,
    starId: Option[Long],
    starTypeId: Option[Long],
    name: String,
    regionName: String,
    regionId: Long,
    constellationName: String,
    constellationId: Long,
    effectTypeId: Option[Long],
    securityClass: Option[String],
    security: Option[Double],
    border: Boolean,
    corridor: Boolean,
    fringe: Boolean,
    hub: Boolean,
    international: Boolean,
    regional: Boolean
)

case class Stargate(id: Long, systemId: Long, toSystemId: Long)
