package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.db.model.*
import org.updraft0.controltower.db.query.ctx.*
import zio.ZIO

import java.sql.SQLException
import javax.sql.DataSource

object sde:
  import schema.*

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

  // inserts
  def insertDogmaAttributeCategories(categories: Vector[DogmaAttributeCategory]): DbOperation[Long] =
    ctx.run(insertAll(schema.dogmaAttributeCategory, categories)).map(_.sum)

  def insertDogmaAttributeTypes(types: Vector[DogmaAttributeType]): DbOperation[Long] =
    ctx.run(insertAll(schema.dogmaAttributeType, types)).map(_.sum)

  def insertItemCategories(itemCategories: Vector[ItemCategory]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemCategory, itemCategories)).map(_.sum)

  def insertItemDogmaAttributes(attrs: Vector[ItemDogmaAttribute]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemDogmaAttribute, attrs)).map(_.sum)

  def insertFactions(factions: Vector[Faction]): DbOperation[Long] =
    ctx.run(insertAll(schema.faction, factions)).map(_.sum)

  def insertItemGroups(itemGroups: Vector[ItemGroup]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemGroup, itemGroups)).map(_.sum)

  def insertItemNames(itemNames: Vector[ItemName]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemName, itemNames)).map(_.sum)

  def insertItemTypes(itemTypes: Vector[ItemType]): DbOperation[Long] =
    ctx.run(insertAll(schema.itemType, itemTypes)).map(_.sum)

  def insertNpcCorporations(corps: Vector[NpcCorporation]): DbOperation[Long] =
    ctx.run(insertAll(schema.npcCorporation, corps)).map(_.sum)

  def insertStationOperations(operations: Vector[StationOperation]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationOperation, operations)).map(_.sum)

  def insertStationOperationServices(services: Vector[StationOperationService]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationOperationService, services)).map(_.sum)

  def insertStationServices(services: Vector[StationService]): DbOperation[Long] =
    ctx.run(insertAll(schema.stationService, services)).map(_.sum)

  def insertStargates(stargates: Vector[Stargate]): DbOperation[Long] =
    ctx.run(insertAll(schema.stargate, stargates)).map(_.sum)

  def insertConstellation(constellation: Constellation): DbOperation[Long] =
    ctx.run(insert(schema.constellation, lift(constellation)))

  def insertSolarSystem(solarSystem: SolarSystem): DbOperation[Long] =
    ctx.run(insert(schema.solarSystem, lift(solarSystem)))

  def insertSolarSystemStar(star: SolarSystemStar): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemStar, lift(star)))

  def insertSolarSystemPlanet(planet: SolarSystemPlanet): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemPlanet, lift(planet)))

  def insertSolarSystemMoon(moon: SolarSystemMoon): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemMoon, lift(moon) /* 🏋️ */ ))

  def insertSolarSystemAsteroidBelt(ab: SolarSystemAsteroidBelt): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemAsteroidBelt, lift(ab)))

  def insertNpcStation(s: NpcStation): DbOperation[Long] =
    ctx.run(insert(schema.npcStation, lift(s)))

  def insertVersion(meta: Option[String]): DbOperation[Int] =
    ctx.run(schema.version.insert(_.createdAt -> unixepoch, _.meta -> lift(meta)).returning(_.id))
