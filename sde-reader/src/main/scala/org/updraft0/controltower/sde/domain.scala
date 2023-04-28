package org.updraft0.controltower.sde

/** Top-level objects that can be extracted from the SDE export directly (with no further processing)
  */
enum ExportedData:
  /** @note
    *   from `fsd/universe/<region>/region.staticdata`
    */
  case Region(id: Long, nameId: Long, tag: String, wormholeClass: Option[WormholeClass], factionId: Option[Long])

  /** @note
    *   from `fsd/universe/<region>/<constellation>/constellation.staticdata`
    */
  case Constellation(id: Long, nameId: Long, tag: String, regionTag: String)

  /** @note
    *   from `fsd/universe/<region>/<constellation>/<solarsystem>/solarsystem.staticdata`
    */
  case SolarSystem(
      // identifiers
      tag: String,
      constellationTag: String,
      regionTag: String,
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

  /** @note
    *   from `fsd/categoryIDs.yaml`
    */
  case CategoryIds(value: Vector[CategoryId])

  /** @note
    *   from `fsd/groupIDs.yaml`
    */
  case GroupIds(value: Vector[GroupId])

  /** @note
    *   from `fsd/typeIDs.yaml`
    */
  case TypeIds(value: Vector[TypeId])

  /** @note
    *   from `fsd/stationServices.yaml`
    */
  case StationServices(value: Vector[StationService])

  /** @note
    *   from `bsd/staStations.yaml`
    */
  case Stations(value: Vector[Station])

  /** @note
    *   from `bsd/invUniqueNames.yaml`
    */
  case UniqueNames(value: Vector[UniqueName])

  case DogmaAttributeCategories(value: Vector[DogmaAttributeCategory])

// -- array types

case class CategoryId(id: Long, nameEn: String, iconId: Option[Long])
case class GroupId(id: Long, categoryId: Long, nameEn: String, iconId: Option[Long])
case class TypeId(id: Long, nameEn: String, groupId: Long, descriptionEn: Option[String])
case class UniqueName(itemId: Long, groupId: Int, name: String)
case class Station(constellationId: Long, regionId: Long, solarSystemId: Long, name: String, corporationId: Long)
case class StationService(id: Long, nameEn: String)

// -- solar system

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

// -- dogma

case class DogmaAttributeCategory(id: Long, name: String, description: Option[String])

// -- own mappings

// TODO: this will probably have to be moved elsewhere...

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
