package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.db.model.*
import zio.Chunk

import java.time.Instant

object sde:
  import auth.given
  import ctx.*

  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  private val BatchRows = 5_000

  private inline def StructureGroupIds =
    Set(365L /* POS */, 1657 /* Citadel */, 1406 /* Refinery */, 1404 /* Engineering complex */ )

  given JsonValueCodec[SdeLoadMeta] = JsonCodecMaker.make

  given MappedEncoding[String, SdeLoadMeta] = MappedEncoding(readFromString[SdeLoadMeta](_))
  given MappedEncoding[SdeLoadMeta, String] = MappedEncoding(writeToString[SdeLoadMeta](_))

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
    inline def itemType                = quote(querySchema[ItemType]("sde.item_type"))
    inline def npcCorporation          = quote(querySchema[NpcCorporation]("sde.npc_corporation"))
    inline def npcStation              = quote(querySchema[NpcStation]("sde.npc_station"))
    inline def region                  = quote(querySchema[Region]("sde.region"))
    inline def solarSystem             = quote(querySchema[SolarSystem]("sde.solar_system"))
    inline def solarSystemEffect       = quote(querySchema[SolarSystemEffect]("sde.solar_system_effect"))
    inline def solarSystemPlanet       = quote(querySchema[SolarSystemPlanet]("sde.solar_system_planet"))
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

  private inline def insertAllChunks[T](inline entity: Quoted[EntityQuery[T]], inline values: Chunk[T]) = quote {
    liftQuery(values).foreach(e => entity.insertValue(e))
  }

  // queries
  def queryStructureTypes: DbOperation[List[ItemType]] =
    ctx.run(quote(schema.itemType.filter(it => liftQuery(StructureGroupIds).contains(it.groupId))))

  // upserts

  // more convenient to upsert the same region multiple times than do the first one
  def upsertRegion(region: Region): DbOperation[Long] =
    ctx.run(insert(schema.region, lift(region)).onConflictIgnore)

  def insertRegions(regions: Chunk[Region]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.region, regions), BatchRows).map(_.sum)

  // inserts
  def insertDogmaAttributeCategories(categories: Chunk[DogmaAttributeCategory]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.dogmaAttributeCategory, categories), BatchRows).map(_.sum)

  def insertDogmaAttributeTypes(types: Chunk[DogmaAttributeType]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.dogmaAttributeType, types), BatchRows).map(_.sum)

  def insertItemCategories(itemCategories: Chunk[ItemCategory]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.itemCategory, itemCategories), BatchRows).map(_.sum)

  def insertItemDogmaAttributes(attrs: Chunk[ItemDogmaAttribute]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.itemDogmaAttribute, attrs), BatchRows).map(_.sum)

  def insertFactions(factions: Chunk[Faction]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.faction, factions), BatchRows).map(_.sum)

  def insertItemGroups(itemGroups: Chunk[ItemGroup]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.itemGroup, itemGroups), BatchRows).map(_.sum)

  def insertItemTypes(itemTypes: Chunk[ItemType]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.itemType, itemTypes), BatchRows).map(_.sum)

  def insertNpcCorporations(corps: Chunk[NpcCorporation]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.npcCorporation, corps), BatchRows).map(_.sum)

  def insertStationOperations(operations: Chunk[StationOperation]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.stationOperation, operations), BatchRows).map(_.sum)

  def insertStationOperationServices(services: Chunk[StationOperationService]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.stationOperationService, services), BatchRows).map(_.sum)

  def insertStationServices(services: Chunk[StationService]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.stationService, services), BatchRows).map(_.sum)

  def insertStargates(stargates: Chunk[Stargate]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.stargate, stargates), BatchRows).map(_.sum)

  def insertConstellation(constellation: Constellation): DbOperation[Long] =
    ctx.run(insert(schema.constellation, lift(constellation)))

  def insertConstellations(constellations: Chunk[Constellation]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.constellation, constellations), BatchRows).map(_.sum)

  def insertSolarSystemEffects(effects: Chunk[SolarSystemEffect]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.solarSystemEffect, effects), BatchRows).map(_.sum)

  def insertSolarSystems(systems: Chunk[SolarSystem]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.solarSystem, systems), BatchRows).map(_.sum)

  def insertSolarSystemStar(star: SolarSystemStar): DbOperation[Long] =
    ctx.run(insert(schema.solarSystemStar, lift(star)))

  def insertSolarSystemStars(stars: Chunk[SolarSystemStar]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.solarSystemStar, stars), BatchRows).map(_.sum)

  def insertSolarSystemPlanets(planets: Chunk[SolarSystemPlanet]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.solarSystemPlanet, planets), BatchRows).map(_.sum)

  def insertNpcStations(stations: Chunk[NpcStation]): DbOperation[Long] =
    ctx.run(insertAllChunks(schema.npcStation, stations), BatchRows).map(_.sum)

  def insertVersion(releasedAt: Instant, buildNumber: Long, meta: SdeLoadMeta): DbOperation[Int] =
    ctx.run(
      quote(
        schema.version
          .insert(
            _.createdAt   -> unixepoch,
            _.releasedAt  -> lift(releasedAt),
            _.buildNumber -> lift(buildNumber),
            _.meta        -> lift(meta)
          )
          .returning(_.id)
      )
    )

  // queries

  def getLatestVersion: DbOperation[Option[Version]] =
    ctx.run(quote(schema.version.sortBy(_.buildNumber)(using Ord.desc).take(1))).map(_.headOption)

  def getRegions: DbOperation[List[Region]] =
    ctx.run(quote(schema.region))

  def getConstellations: DbOperation[List[Constellation]] =
    ctx.run(quote(schema.constellation))

  def getSolarSystem: DbOperation[List[SolarSystem]] =
    ctx.run(quote(schema.solarSystem))

  def getSolarSystemEffects: DbOperation[List[SolarSystemEffect]] =
    ctx.run(quote(schema.solarSystemEffect))

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

  def deleteSolarSystemEffect: DbOperation[Long] =
    ctx.run(schema.solarSystemEffect.delete)

  def deleteSolarSystemPlanet: DbOperation[Long] =
    ctx.run(schema.solarSystemPlanet.delete)

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

  // backfills (manual sql)

  // see https://github.com/zio/zio-quill/issues/1458
  def updateSolarSystemColumns: DbOperation[Long] =
    ctx.run(quote(infix"""
        update sde.solar_system
        set region_name        = (select r.name from sde.region r where r.id = solar_system.region_id),
            constellation_name = (select c.name from sde.constellation c where c.id = solar_system.constellation_id),
            effect_type_id     = (select e.type_id from sde.solar_system_effect e where e.system_id = solar_system.id)
           """.as[Action[Int]]))

  def updateSolarSystemWhClassFromRegion: DbOperation[Long] =
    ctx.run(quote(infix"""
        update sde.solar_system
        set wh_class_id = (select r.wh_class_id from sde.region r where r.id = solar_system.region_id)
        where wh_class_id is null;
             """.as[Action[Int]]))

  def updateConstellationRegionName: DbOperation[Long] =
    ctx.run(
      quote(
        infix"""
        update sde.constellation
          set region_name = (select r.name from sde.region r where r.id = constellation.region_id);
           """.as[Action[Int]]
      )
    )

  // TODO: not particularly clean, does not handle roman numerals in planet name
  def updateNpcStationName: DbOperation[Long] =
    ctx.run(
      quote(
        infix"""
        update sde.npc_station
        set name =
                (select s.name from sde.solar_system s where s.id = npc_station.system_id) ||
                ' ' ||
                printf('%i', npc_station.celestial_index) ||
                case
                    when npc_station.orbit_index is null then ''
                    else printf(' - Moon %i', npc_station.orbit_index)
                    end ||
                ' - ' ||
                (select c.name from sde.npc_corporation c where c.id = npc_station.owner_id) ||
                ' ' ||
                (select o.name from sde.station_operation o where o.id = npc_station.operation_id)
           """.as[Action[Int]]
      )
    )

  def vacuumSde: DbOperation[Long] =
    // TODO this is a bit strange
    inline val vac = "VACUUM sde;"
    ctx.run(quote(infix"#$vac".as[Action[Unit]]))
