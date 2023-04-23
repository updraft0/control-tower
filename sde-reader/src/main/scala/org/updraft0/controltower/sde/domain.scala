package org.updraft0.controltower.sde

// Minimal domain model needed to import routing-relevant information
// Each record here is derivable from a single YAML file in the SDE

type DomainTopLevel = Region | Constellation | SolarSystem | Vector[TypeId] | Vector[Station] | Vector[ItemName] |
  Vector[StationService]

enum SpaceType:
  case Known
  case Wormhole
  case Pochven
  case Abyssal
  case Internal

enum WormholeClass(val value: Int, val spaceType: SpaceType):
  // "normal" w-space
  case C1 extends WormholeClass(1, SpaceType.Wormhole)
  case C2 extends WormholeClass(2, SpaceType.Wormhole)
  case C3 extends WormholeClass(3, SpaceType.Wormhole)
  case C4 extends WormholeClass(4, SpaceType.Wormhole)
  case C5 extends WormholeClass(5, SpaceType.Wormhole)
  case C6 extends WormholeClass(6, SpaceType.Wormhole)
  // k-space
  case Hi   extends WormholeClass(7, SpaceType.Known)
  case Lo   extends WormholeClass(8, SpaceType.Known)
  case Null extends WormholeClass(9, SpaceType.Known)
  // internal
  case Internal10 extends WormholeClass(10, SpaceType.Internal)
  case Internal11 extends WormholeClass(11, SpaceType.Internal)
  // thera
  case Thera extends WormholeClass(12, SpaceType.Known)
  // frig shattered
  case ShatteredFrig extends WormholeClass(13, SpaceType.Wormhole)
  // drifter
  case SentinelDrifter extends WormholeClass(14, SpaceType.Wormhole)
  case BarbicanDrifter extends WormholeClass(15, SpaceType.Wormhole)
  case VidetteDrifter  extends WormholeClass(16, SpaceType.Wormhole)
  case ConfluxDrifter  extends WormholeClass(17, SpaceType.Wormhole)
  case RedoubtDrifter  extends WormholeClass(18, SpaceType.Wormhole)
  // abyssal space
  case Void      extends WormholeClass(19, SpaceType.Abyssal)
  case Abyssal20 extends WormholeClass(20, SpaceType.Abyssal)
  case Abyssal21 extends WormholeClass(21, SpaceType.Abyssal)
  case Abyssal22 extends WormholeClass(22, SpaceType.Abyssal)
  case Abyssal23 extends WormholeClass(23, SpaceType.Abyssal)
  // pochven
  case Pochven extends WormholeClass(25, SpaceType.Pochven)

enum WormholeEffect(val typeId: Long):
  case Magnetar    extends WormholeEffect(30574)
  case BlackHole   extends WormholeEffect(30575)
  case RedGiant    extends WormholeEffect(30576)
  case Pulsar      extends WormholeEffect(30577)
  case Cataclysmic extends WormholeEffect(30670)
  case WolfRayet   extends WormholeEffect(30669)

/*
 * from `fsd/universe/<region>/<constellation>/constellation.staticdata`
 */
case class Constellation(id: Long, nameId: Long, name: String, region: String)

/*
 * from `fsd/universe/<region>/region.staticdata`
 */
case class Region(id: Long, nameId: Long, name: String, wormholeClass: Option[WormholeClass], factionId: Option[Long])

/*
 * from `fsd/universe/<region>/<constellation>/<solarsystem>/solarsystem.staticdata`
 */
case class SolarSystem(
    // identifiers
    name: String,
    constellation: String,
    region: String,
    // key parameters
    id: Long,
    nameId: Long,
    star: Option[Star],
    secondaryEffect: Option[WormholeEffect],
    // planets + stargates
    planets: Vector[Planet],
    stargates: Vector[Stargate],
    // security
    securityClass: Option[String],
    security: Option[Double],
    // flags
    border: Boolean,
    corridor: Boolean,
    fringe: Boolean,
    hub: Boolean,
    international: Boolean,
    regional: Boolean
)

case class NpcStation(id: Long, ownerId: Long, typeId: Long)
case class PlanetMoon(id: Long, npcStations: Vector[NpcStation])
case class PlanetAsteroidBelt(id: Long)

case class Planet(
    id: Long,
    index: Int,
    typeId: Long,
    moons: Vector[PlanetMoon],
    asteroidBelts: Vector[PlanetAsteroidBelt]
)
case class Star(id: Long, typeId: Long)
case class Stargate(id: Long, destinationId: Long)

/** from `fsd/typeIDs.yaml`
  */
case class TypeId(id: Long, descriptionEn: String, nameEn: String, groupId: Long, graphicId: Long)

/** from `fsd/stationServices.yaml`
  */
case class StationService(id: Long, nameEn: String)

/** from `bsd/staStations.yaml`
  */
case class Station(constellationId: Long, regionId: Long, solarSystemId: Long, name: String, corporationId: Long)

/** from `bsd/invNames.yaml`
  */
case class ItemName(id: Long, name: String)
