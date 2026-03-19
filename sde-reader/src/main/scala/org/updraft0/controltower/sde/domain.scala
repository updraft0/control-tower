package org.updraft0.controltower.sde

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.updraft0.controltower.constant
import zio.Chunk

case class LanguageCode(value: String) extends AnyVal

/** Top-level objects that can be extracted from the SDE export directly (with no further processing)
  */
enum ExportedData:

  /** @note
    *   from `mapConstellations.jsonl`
    */
  case Constellations(value: Chunk[Constellation])

  /** @note
    *   from `mapSolarSystems.jsonl`
    */
  case SolarSystem(
      @named("_key") id: constant.SystemId,
      @named("constellationID") constellationId: Long,
      @named("regionID") regionId: Long,
      @named("name") nameMap: Map[String, String],
      @named("planetIDs") planets: Vector[Long],
      radius: Double,
      securityStatus: Option[Double],
      @named("starID") starId: Option[Long],
      @named("wormholeClassID") whClassId: Option[Int]
  )

  /** @note
    *   from `categories.jsonl`
    */
  case CategoryIds(value: Chunk[CategoryId])

  /** @note
    *   from `dogmaAttributeCategories.jsonl`
    */
  case DogmaAttributeCategories(value: Chunk[DogmaAttributeCategory])

  /** @note
    *   from `dogmaAttributes.jsonl`
    */
  case DogmaAttributes(value: Chunk[DogmaAttribute])

  /** @note
    *   from `factions.jsonl`
    */
  case Factions(value: Chunk[Faction])

  /** @note
    *   from `groups.jsonl`
    */
  case GroupIds(value: Chunk[GroupId])

  /** @note
    *   from `npcCorporations.jsonl`
    */
  case NpcCorporations(value: Chunk[NpcCorporation])

  /** @note
    *   from `npcStations.jsonl`
    */
  case NpcStations(value: Chunk[NpcStation])

  /** @note
    *   from `mapPlanets.jsonl`
    */
  case Planet(
      @named("_key") id: Long,
      celestialIndex: Int,
      @named("moonIDs") moonIds: Option[Vector[Long]],
      radius: Double,
      @named("solarSystemID") solarSystemId: constant.SystemId,
      @named("typeID") typeId: constant.TypeId
  )

  /** @note
    *   from `mapRegions.jsonl`
    */
  case Regions(value: Chunk[Region])

  /** @note
    *   from `mapSecondarySuns.jsonl`
    */
  case SecondarySuns(value: Chunk[SecondarySun])

  /** @note
    *   from `mapStargates.jsonl`
    */
  case Stargate(
      @named("_key") id: Long,
      destination: StargateDestination,
      @named("solarSystemID") solarSystemId: constant.SystemId,
      @named("typeID") typeId: constant.TypeId
  )

  /** @note
    *   from `mapStars.jsonl`
    */
  case Star(
      @named("_key") id: Long,
      radius: Double,
      @named("solarSystemID") solarSystemId: constant.SystemId,
      @named("typeID") typeId: constant.TypeId
  )

  /** @note
    *   from `stationOperations.jsonl`
    */
  case StationOperations(value: Chunk[StationOperation])

  /** @note
    *   from `stationServices.jsonl`
    */
  case StationServices(value: Chunk[StationService])

  /** @note
    *   from `typeDogmas.jsonl`
    */
  case TypeDogmas(value: Chunk[TypeDogma])

  /** @note
    *   from `types.jsonl`
    */
  case TypeId(
      @named("_key") id: constant.TypeId,
      @named("name") nameMap: Map[String, String],
      @named("groupID") groupId: Long,
      volume: Option[Double],
      mass: Option[Double]
  )

// -- array/map-of-object types

case class CategoryId(
    @named("_key") id: Long,
    @named("name") nameMap: Map[String, String],
    @named("iconID") iconId: Option[Long]
)

case class Constellation(
    @named("_key") id: Long,
    @named("name") nameMap: Map[String, String],
    @named("factionID") factionId: Option[Long],
    @named("regionID") regionId: Long,
    @named("solarSystemIDs") solarSystemIds: Vector[constant.SystemId],
    @named("wormholeClassID") wormholeClassId: Option[constant.WormholeClass]
)

case class DogmaAttributeCategory(@named("_key") id: Long, name: String, description: Option[String])
case class DogmaAttribute(
    @named("_key") id: Long,
    @named("attributeCategoryID") categoryId: Option[Long],
    dataType: Int,
    name: String,
    description: Option[String],
    defaultValue: Double,
    @named("unitID") unitId: Option[Int],
    @named("iconID") iconId: Option[Long]
)
case class Faction(
    @named("_key") id: Long,
    @named("name") nameMap: Map[String, String],
    @named("corporationID") corporationId: Option[constant.CorporationId],
    @named("iconID") iconId: Long,
    @named("militiaCorporationID") militiaCorporationId: Option[constant.CorporationId],
    memberRaces: Vector[Int],
    sizeFactor: Double,
    @named("solarSystemID") solarSystemId: constant.SystemId,
    uniqueName: Boolean
)
case class GroupId(
    @named("_key") id: Long,
    @named("name") nameMap: Map[String, String],
    @named("categoryID") categoryId: Long,
    @named("iconID") iconId: Option[Long]
)

case class NpcCorporation(
    @named("_key") id: constant.CorporationId,
    @named("name") nameMap: Map[String, String],
    @named("allowedMemberRaces") allowedRaces: Option[Vector[Long]],
    @named("ceoID") ceoId: Option[Long],
    @named("raceID") raceId: Option[Int],
    @named("factionID") factionId: Option[Long],
    @named("iconID") iconId: Option[Long],
    @named("solarSystemID") solarSystemId: Option[constant.SystemId],
    @named("stationID") stationId: Option[Long],
    @named("tickerName") ticker: String,
    uniqueName: Boolean
)

/** @note
  *   from `mapRegions.jsonl`
  */
case class Region(
    @named("_key") id: Long,
    @named("name") nameMap: Map[String, String],
    @named("constellationIDs") constellationIds: Vector[Long],
    @named("wormholeClassID") wormholeClassId: Option[constant.WormholeClass],
    @named("factionID") factionId: Option[Long]
)

case class TypeDogma(
    @named("_key") id: constant.TypeId,
    @named("dogmaAttributes") attributes: Vector[DogmaAttributePair],
    @named("dogmaEffects") effects: Vector[DogmaEffectPair]
)

case class DogmaAttributePair(@named("attributeID") id: Int, value: Double)
case class DogmaEffectPair(@named("effectID") id: Int, isDefault: Boolean)

case class StationOperation(
    @named("_key") id: Long,
    @named("operationName") nameMap: Map[String, String],
    @named("activityID") activityId: Int,
    services: Vector[Int],
    stationTypes: Vector[KeyValuePair[Int, Int]]
)
case class StationService(@named("_key") id: Long, @named("serviceName") nameMap: Map[String, String])

// -- solar system

case class NpcStation(
    @named("_key") id: Long,
    @named("solarSystemID") systemId: constant.SystemId,
    @named("typeID") typeId: constant.TypeId,
    @named("ownerID") ownerId: Long,
    @named("operationID") operationId: Int,
    celestialIndex: Option[Int],
    @named("orbitID") orbitId: Long,
    orbitIndex: Option[Int]
)

// wormhole effects
case class SecondarySun(
    @named("_key") id: Long,
    @named("effectBeaconTypeID") beaconId: Long,
    @named("solarSystemID") solarSystemId: constant.SystemId,
    @named("typeID") effectId: constant.WormholeEffect
)

case class StargateDestination(
    @named("solarSystemID") solarSystemId: constant.SystemId,
    @named("stargateID") stargateId: Long
)

case class KeyValuePair[K, V](@named("_key") key: K, @named("_value") value: V)
