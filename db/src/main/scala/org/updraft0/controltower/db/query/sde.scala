package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.db.model.*
import zio.ZIO

import java.sql.SQLException
import javax.sql.DataSource

object sde:
  import auth.given
  import schema.*
  import ctx.*
  import zio.json.*

  private val BatchRows = 5_000

  given JsonCodec[SdeLoadMeta] = JsonCodec.derived

  given MappedEncoding[String, SdeLoadMeta] = MappedEncoding(
    _.fromJson[SdeLoadMeta].getOrElse(
      throw new RuntimeException("Unable to decode SdeLoadMeta from string")
    )
  )
  given MappedEncoding[SdeLoadMeta, String] = MappedEncoding(_.toJson)

  /** Each table lives in the `sde` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def constellation           = quote(querySchema[Constellation]("sde.constellation"))
    inline def dogmaAttributeCategory  = quote(querySchema[DogmaAttributeCategory]("sde.dogma_attribute_category"))
    inline def dogmaAttributeType      = quote(querySchema[DogmaAttributeType]("sde.dogma_attribute_type"))
    inline def faction                 = quote(querySchema[Faction]("sde.faction"))
    inline def itemCategory            = quote(querySchema[ItemCategory]("sde.item_category"))
    inline def itemDogmaAttribute      = quote(querySchema[ItemDogmaAttribute]("sde.item_dogma_attribute"))
    inline def itemGroup               = quote(querySchema[ItemGroup]("sde.item_group"))
    inline def itemName                = quote(querySchema[ItemName]("sde.item_name"))
    inline def itemType                = quote(querySchema[ItemType]("sde.item_type"))
    inline def npcCorporation          = quote(querySchema[NpcCorporation]("sde.npc_corporation"))
    inline def npcStation              = quote(querySchema[NpcStation]("sde.npc_station"))
    inline def region                  = quote(querySchema[Region]("sde.region"))
    inline def solarSystem             = quote(querySchema[SolarSystem]("sde.solar_system"))
    inline def solarSystemAsteroidBelt = quote(querySchema[SolarSystemAsteroidBelt]("sde.solar_system_asteroid_belt"))
    inline def solarSystemPlanet       = quote(querySchema[SolarSystemPlanet]("sde.solar_system_planet"))
    inline def solarSystemMoon         = quote(querySchema[SolarSystemMoon]("sde.solar_system_moon"))
    inline def solarSystemStar         = quote(querySchema[SolarSystemStar]("sde.solar_system_star"))
    inline def stargate                = quote(querySchema[Stargate]("sde.stargate"))
    inline def stationOperation        = quote(querySchema[StationOperation]("sde.station_operation"))
    inline def stationOperationService = quote(querySchema[StationOperationService]("sde.station_operation_service"))
    inline def stationService          = quote(querySchema[StationService]("sde.station_service"))
    inline def version                 = quote(querySchema[Version]("sde.version"))

  private inline def insert[T](inline entity: Quoted[EntityQuery[T]], inline value: T): Insert[T] = quote {
    entity.insertValue(value)
  }

  private inline def insertAll[T](inline entity: Quoted[EntityQuery[T]], inline values: Vector[T]) = quote {
    liftQuery(values).foreach(e => entity.insertValue(e))
  }

  // upserts

  // more convenient to upsert the same region multiple times than do the first one
  def upsertRegion(region: Region): DbOperation[Long] =
    ctx.run(insert(schema.region, lift(region)).onConflictIgnore)

  def upsertItemName(name: ItemName): DbOperation[Long] =
    ctx.run(insert(schema.itemName, lift(name)).onConflictIgnore)

  // inserts
  def insertDogmaAttributeCategories(categories: Vector[DogmaAttributeCategory]): DbOperation[Long] =
    ctx.run(insertAll(schema.dogmaAttributeCategory, categories), BatchRows).map(_.sum)

  def insertDogmaAttributeTypes(types: Vector[DogmaAttributeType]): DbOperation[Long] =
    ctx.run(insertAll(schema.dogmaAttributeType, types), BatchRows).map(_.sum)

  def insertItemCategories(itemCategories: Vector[ItemCategory]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemCategory, itemCategories), BatchRows).map(_.sum)

  def insertItemDogmaAttributes(attrs: Vector[ItemDogmaAttribute]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemDogmaAttribute, attrs), BatchRows).map(_.sum)

  def insertFactions(factions: Vector[Faction]): DbOperation[Long] =
    ctx.run(insertAll(schema.faction, factions), BatchRows).map(_.sum)

  def insertItemGroups(itemGroups: Vector[ItemGroup]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemGroup, itemGroups), BatchRows).map(_.sum)

  def insertItemNames(itemNames: Vector[ItemName]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemName, itemNames), BatchRows).map(_.sum)

  def insertItemTypes(itemTypes: Vector[ItemType]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemType, itemTypes), BatchRows).map(_.sum)

  def insertNpcCorporations(corps: Vector[NpcCorporation]): DbOperation[Long] =
    ctx.run(insertAll(schema.npcCorporation, corps), BatchRows).map(_.sum)

  def insertStationOperations(operations: Vector[StationOperation]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationOperation, operations), BatchRows).map(_.sum)

  def insertStationOperationServices(services: Vector[StationOperationService]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationOperationService, services), BatchRows).map(_.sum)

  def insertStationServices(services: Vector[StationService]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationService, services), BatchRows).map(_.sum)

  def insertStargates(stargates: Vector[Stargate]): DbOperation[Long] =
    ctx.run(insertAll(schema.stargate, stargates), BatchRows).map(_.sum)

  def insertConstellation(constellation: Constellation): DbOperation[Long] =
    ctx.run(insert(schema.constellation, lift(constellation)))

  def insertSolarSystem(solarSystem: SolarSystem): DbOperation[Long] =
    ctx.run(insert(schema.solarSystem, lift(solarSystem)))

  def insertSolarSystemStar(star: SolarSystemStar): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemStar, lift(star)))

  def insertSolarSystemPlanets(planets: Vector[SolarSystemPlanet]): DbOperation[Long] =
    ctx.run(insertAll(schema.solarSystemPlanet, planets), BatchRows).map(_.sum)

  def insertSolarSystemMoons(moons: Vector[SolarSystemMoon]): DbOperation[Long] =
    ctx.run(insertAll(schema.solarSystemMoon, moons), BatchRows).map(_.sum)

  def insertSolarSystemAsteroidBelts(belts: Vector[SolarSystemAsteroidBelt]): DbOperation[Long] =
    ctx.run(insertAll(schema.solarSystemAsteroidBelt, belts), BatchRows).map(_.sum)

  def insertNpcStations(stations: Vector[NpcStation]): DbOperation[Long] =
    ctx.run(insertAll(schema.npcStation, stations), BatchRows).map(_.sum)

  def insertVersion(meta: SdeLoadMeta): DbOperation[Int] =
    ctx.run(quote(schema.version.insert(_.createdAt -> unixepoch, _.meta -> lift(meta)).returning(_.id)))

  // queries

  def getLatestVersion: DbOperation[Option[Version]] =
    ctx.run(quote(schema.version.sortBy(_.createdAt)(Ord.desc).take(1))).map(_.headOption)

  def getRegions: DbOperation[List[Region]] =
    ctx.run(quote(schema.region))

  def getConstellations: DbOperation[List[Constellation]] =
    ctx.run(quote(schema.constellation))

  def getSolarSystem: DbOperation[List[SolarSystem]] =
    ctx.run(quote(schema.solarSystem))

  // deletes
  def deleteConstellation: DbOperation[Long] =
    ctx.run(schema.constellation.delete)

  def deleteDogmaAttributeCategory: DbOperation[Long] =
    ctx.run(schema.dogmaAttributeCategory.delete)

  def deleteDogmaAttributeType: DbOperation[Long] =
    ctx.run(schema.dogmaAttributeType.delete)

  def deleteFaction: DbOperation[Long] =
    ctx.run(schema.faction.delete)

  def deleteItemCategory: DbOperation[Long] =
    ctx.run(schema.itemCategory.delete)

  def deleteItemDogmaAttribute: DbOperation[Long] =
    ctx.run(schema.itemDogmaAttribute.delete)

  def deleteItemGroup: DbOperation[Long] =
    ctx.run(schema.itemGroup.delete)

  def deleteItemName: DbOperation[Long] =
    ctx.run(schema.itemName.delete)

  def deleteItemType: DbOperation[Long] =
    ctx.run(schema.itemType.delete)

  def deleteNpcCorporation: DbOperation[Long] =
    ctx.run(schema.npcCorporation.delete)

  def deleteNpcStation: DbOperation[Long] =
    ctx.run(schema.npcStation.delete)

  def deleteRegion: DbOperation[Long] =
    ctx.run(schema.region.delete)

  def deleteSolarSystem: DbOperation[Long] =
    ctx.run(schema.solarSystem.delete)

  def deleteSolarSystemAsteroidBelt: DbOperation[Long] =
    ctx.run(schema.solarSystemAsteroidBelt.delete)

  def deleteSolarSystemPlanet: DbOperation[Long] =
    ctx.run(schema.solarSystemPlanet.delete)

  def deleteSolarSystemMoon: DbOperation[Long] =
    ctx.run(schema.solarSystemMoon.delete)

  def deleteSolarSystemStar: DbOperation[Long] =
    ctx.run(schema.solarSystemStar.delete)

  def deleteStargate: DbOperation[Long] =
    ctx.run(schema.stargate.delete)

  def deleteStationOperation: DbOperation[Long] =
    ctx.run(schema.stationOperation.delete)

  def deleteStationOperationService: DbOperation[Long] =
    ctx.run(schema.stationOperationService.delete)

  def deleteStationService: DbOperation[Long] =
    ctx.run(schema.stationService.delete)

  def vacuumSde: DbOperation[Long] =
    // TODO this is a bit strange
    val vac = "VACUUM sde;"
    ctx.run(quote(infix"#$vac".as[Action[Unit]]))
