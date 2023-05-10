package org.updraft0.controltower.sde

import org.updraft0.controltower.constant.*

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
    *   from `fsd/dogmaAttributeCategories.yaml`
    */
  case DogmaAttributeCategories(value: Vector[DogmaAttributeCategory])

  /** @note
    *   from `fsd/dogmaAttributes.yaml`
    */
  case DogmaAttributes(value: Vector[DogmaAttribute])

  /** @note
    *   from `fsd/factions.yaml`
    */
  case Factions(value: Vector[Faction])

  /** @note
    *   from `fsd/groupIDs.yaml`
    */
  case GroupIds(value: Vector[GroupId])

  /** @note
    *   from `fsd/npcCorporations.yaml`
    */
  case NpcCorporations(value: Vector[NpcCorporation])

  /** @note
    *   from `fsd/stationOperations.yaml`
    */
  case StationOperations(value: Vector[StationOperation])

  /** @note
    *   from `fsd/stationServices.yaml`
    */
  case StationServices(value: Vector[StationService])

  /** @note
    *   from `fsd/typeDogma.yaml`
    */
  case TypeDogmas(value: Vector[TypeDogma])

  /** @note
    *   from `fsd/typeIDs.yaml`
    */
  case TypeIds(value: Vector[TypeId])

  /** @note
    *   from `bsd/invUniqueNames.yaml`
    */
  case UniqueNames(value: Vector[UniqueName])

// -- array/map-of-object types

case class CategoryId(id: Long, nameEn: String, iconId: Option[Long])
case class DogmaAttributeCategory(id: Long, name: String, description: Option[String])
case class DogmaAttribute(
    id: Long,
    categoryId: Option[Long],
    dataType: Int,
    name: String,
    description: Option[String],
    defaultValue: Double,
    unitId: Option[Int],
    iconId: Option[Long]
)
case class Faction(
    id: Long,
    nameEn: String,
    corporationId: Option[Long],
    descriptionEn: String,
    shortDescriptionEn: Option[String],
    iconId: Long,
    militiaCorporationId: Option[Long],
    memberRaces: Vector[Int],
    sizeFactor: Double,
    solarSystemId: Long,
    uniqueName: Boolean
)
case class GroupId(id: Long, categoryId: Long, nameEn: String, iconId: Option[Long])
case class NpcCorporation(
    id: Long,
    nameEn: String,
    allowedRaces: Option[Vector[Long]],
    ceoId: Option[Long],
    raceId: Option[Int],
    descriptionEn: Option[String],
    factionId: Option[Long],
    iconId: Option[Long],
    solarSystemId: Option[Long],
    stationId: Option[Long],
    ticker: String,
    uniqueName: Boolean
)
case class TypeId(
    id: Long,
    nameEn: String,
    groupId: Long,
    descriptionEn: Option[String],
    mass: Option[Double],
    volume: Option[Double]
)
case class TypeDogma(id: Long, attributes: Map[Long, Double], effects: Map[Long, Boolean])
case class StationOperation(
    id: Long,
    activityId: Int,
    nameEn: String,
    descriptionEn: Option[String],
    services: Vector[Int],
    stationTypes: Map[Int, Int]
)
case class StationService(id: Long, nameEn: String)
case class UniqueName(itemId: Long, groupId: Int, name: String)

// -- solar system

case class NpcStation(id: Long, ownerId: Long, typeId: Long, operationId: Long)
case class PlanetMoon(id: Long, npcStations: Vector[NpcStation])
case class PlanetAsteroidBelt(id: Long)

case class Planet(
    id: Long,
    index: Int,
    typeId: Long,
    moons: Vector[PlanetMoon],
    asteroidBelts: Vector[PlanetAsteroidBelt],
    stations: Vector[NpcStation]
)
case class Star(id: Long, typeId: Long)
case class Stargate(id: Long, destinationId: Long)
