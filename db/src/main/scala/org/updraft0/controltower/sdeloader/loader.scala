package org.updraft0.controltower.sdeloader

import org.updraft0.controltower.db.model.*
import org.updraft0.controltower.db.query
import org.updraft0.controltower.sde
import org.updraft0.controltower.sde.{ExportedData, GroupedExport, parser}
import zio.*
import zio.stream.ZStream

import javax.sql.DataSource
import scala.annotation.switch

// fixme how to recover better error handling?
case class ParserException(message: String, cause: parser.Error) extends RuntimeException(message)

private[sdeloader] enum ImportState:
  case Initial(uniqueNames: Option[ExportedData.UniqueNames] = None)
  case ReadyForSolarSystems(uniqueNamesById: Map[Long, sde.UniqueName])

/** Loads the whole of the SDE (group) data into the db
  */
def intoDb(sde: ZStream[Any, parser.Error, GroupedExport]): RIO[DataSource, Chunk[Long]] =
  query.transaction(
    sde
      .mapError(e => ParserException(s"Parser failed: $e", e))
      .mapAccumZIO(ImportState.Initial(): ImportState) {
        case (i: ImportState.Initial, GroupedExport.Ungrouped(names: ExportedData.UniqueNames)) =>
          loadSingle(names)
            .map(res => nextState(i.copy(uniqueNames = Some(names))) -> res)
        case (_: ImportState.Initial, _: GroupedExport.RegionSolarSystems) =>
          unsupported("BUG: Not seen all the data required to load solar systems yet")
        case (r: ImportState.ReadyForSolarSystems, rss: GroupedExport.RegionSolarSystems) =>
          loadSolarSystems(r, rss).map(res => r -> res)
        case (s, GroupedExport.Ungrouped(other)) =>
          loadSingle(other).map(res => s -> res)
      }
      .runCollect
  )

private[sdeloader] def nextState(state: ImportState): ImportState = state match {
  case ImportState.Initial(Some(uniqueNames)) =>
    ImportState.ReadyForSolarSystems(uniqueNamesById = uniqueNames.value.map(n => n.itemId -> n).toMap)
  case _ =>
    state
}

private[sdeloader] def loadSingle(raw: ExportedData): RIO[DataSource, Long] = raw match {
//  case ExportedData.Stations(stations) => query.sde.insertStations(stations.map(toStation))
  case ExportedData.CategoryIds(categoryIds) => query.sde.insertItemCategories(categoryIds.map(toItemCategory))
  case ExportedData.DogmaAttributeCategories(categories) =>
    query.sde.insertDogmaAttributeCategories(categories.map(toDogmaAttributeCategory))
  case ExportedData.GroupIds(groupIds) => query.sde.insertItemGroups(groupIds.map(toItemGroup))
  case ExportedData.StationServices(stationServices) =>
    query.sde.insertStationServices(stationServices.map(toStationService))
  case ExportedData.TypeIds(ids)       => query.sde.insertItemTypes(ids.map(toItemType))
  case ExportedData.UniqueNames(names) => query.sde.insertItemNames(names.map(toItemName))
//  case other                           => ZIO.logError(s"FIXME UNSUPPORTED ${other.getClass.getSimpleName}").as(0L)
//  case other                           => unsupported(s"BUG: Cannot load a single ${other.getClass.getSimpleName}")

  case _: ExportedData.Region | _: ExportedData.Constellation | _: ExportedData.SolarSystem =>
    unsupported("BUG: cannot import a single region/constellation/solar system standalone")
}

private[sdeloader] def loadSolarSystems(s: ImportState.ReadyForSolarSystems, rss: GroupedExport.RegionSolarSystems) =
  for
    rc <- query.sde.upsertRegion(toRegion(s.uniqueNamesById, rss.region))
    cc <- query.sde.insertConstellation(toConstellation(s.uniqueNamesById, rss.region, rss.constellation))
    sc <- ZIO
      .foreach(rss.solarSystems)(insertSolarSystem(s.uniqueNamesById, rss.region, rss.constellation, _))
      .map(_.sum)
  yield rc + cc + sc

private def insertSolarSystem(
    names: Map[Long, sde.UniqueName],
    r: ExportedData.Region,
    c: ExportedData.Constellation,
    s: ExportedData.SolarSystem
) =
  for
    systemCount <- query.sde.insertSolarSystem(toSolarSystem(names, r, c, s))
    starCount <- s.star
      .map(ss => query.sde.insertSolarSystemStar(SolarSystemStar(ss.id, ss.typeId)))
      .getOrElse(ZIO.succeed(0L))
    planetCount   <- ZIO.foreach(s.planets)(p => query.sde.insertSolarSystemPlanet(toPlanet(s, p))).map(_.sum)
    moonCount     <- insertSolarSystemMoons(s)
    beltCount     <- insertAsteroidBelts(s)
    stargateCount <- query.sde.insertStargates(s.stargates.map(sg => toStargate(s, sg)))
    stationCount  <- insertNpcStations(names, s)
  yield systemCount + starCount + planetCount + moonCount + beltCount + stargateCount + stationCount

// FIXME: moon index is not working correctly
private def insertSolarSystemMoons(s: ExportedData.SolarSystem) =
  ZIO
    .foreach(s.planets.flatMap(p => p.moons.zipWithIndex.map((planetMoon, moonIdx) => (p, planetMoon, moonIdx))))(
      (planet, planetMoon, moonIdx) => query.sde.insertSolarSystemMoon(toPlanetMoon(s, planet, planetMoon, moonIdx + 1))
    )
    .map(_.sum)

private def insertNpcStations(names: Map[Long, sde.UniqueName], s: ExportedData.SolarSystem) =
  ZIO
    .foreach(s.planets.flatMap(p => p.moons.flatMap(m => m.npcStations.map(ns => (m, ns)))))((m, ns) =>
      query.sde.insertNpcStation(toNpcStation(names, s, m, ns))
    )
    .map(_.sum)

private def insertAsteroidBelts(s: ExportedData.SolarSystem) =
  ZIO
    .foreach(s.planets.flatMap(p => p.asteroidBelts.map(ab => (p, ab))))((p, ab) =>
      query.sde.insertSolarSystemAsteroidBelt(toAsteroidBelt(s, p, ab))
    )
    .map(_.sum)

private def toRegion(names: Map[Long, sde.UniqueName], r: ExportedData.Region) =
  Region(
    id = r.id,
    name = names(r.id).name,
    whClassId = r.wormholeClass.map(_.value),
    factionId = r.factionId
  )

private def toConstellation(
    names: Map[Long, sde.UniqueName],
    r: ExportedData.Region,
    c: ExportedData.Constellation
) =
  Constellation(
    id = c.id,
    name = names(c.id).name,
    regionId = r.id,
    regionName = names(r.id).name
  )

private def toSolarSystem(
    names: Map[Long, sde.UniqueName],
    r: ExportedData.Region,
    c: ExportedData.Constellation,
    s: ExportedData.SolarSystem
) =
  SolarSystem(
    id = s.id,
    name = names(s.id).name,
    starId = s.star.map(_.id),
    starTypeId = s.star.map(_.typeId),
    regionName = names(r.id).name,
    regionId = r.id,
    constellationName = names(c.id).name,
    constellationId = c.id,
    effectTypeId = s.secondaryEffect.map(_.typeId),
    securityClass = s.securityClass,
    security = s.security,
    border = s.border,
    corridor = s.corridor,
    fringe = s.fringe,
    hub = s.hub,
    international = s.international,
    regional = s.regional
  )

private def toPlanet(s: ExportedData.SolarSystem, p: sde.Planet) = SolarSystemPlanet(p.id, s.id, p.index, p.typeId)
private def toPlanetMoon(s: ExportedData.SolarSystem, p: sde.Planet, m: sde.PlanetMoon, idx: Int) =
  SolarSystemMoon(m.id, p.id, s.id, idx)
private def toAsteroidBelt(s: ExportedData.SolarSystem, p: sde.Planet, ab: sde.PlanetAsteroidBelt) =
  SolarSystemAsteroidBelt(ab.id, p.id, s.id)
private def toStargate(s: ExportedData.SolarSystem, sg: sde.Stargate) = Stargate(sg.id, s.id, sg.destinationId)
private def toNpcStation(
    names: Map[Long, sde.UniqueName],
    s: ExportedData.SolarSystem,
    m: sde.PlanetMoon,
    ns: sde.NpcStation
) =
  NpcStation(
    id = ns.id,
    name = names(ns.id).name,
    ownerId = ns.ownerId,
    typeId = ns.typeId,
    moonId = m.id,
    systemId = s.id
  )

private def toDogmaAttributeCategory(c: sde.DogmaAttributeCategory) =
  DogmaAttributeCategory(c.id, c.name, c.description)

private def toItemCategory(ci: sde.CategoryId): ItemCategory = ItemCategory(ci.id, ci.nameEn, ci.iconId)
private def toItemGroup(gi: sde.GroupId): ItemGroup          = ItemGroup(gi.id, gi.categoryId, gi.nameEn, gi.iconId)
private def toItemName(un: sde.UniqueName): ItemName         = ItemName(un.itemId, un.groupId, un.name)
private def toItemType(ti: sde.TypeId): ItemType             = ItemType(ti.id, ti.nameEn, ti.groupId, ti.descriptionEn)
private def toStationService(ss: sde.StationService): StationService = StationService(ss.id, ss.nameEn)

private def unsupported(message: String): RIO[Any, Nothing] = ZIO.fail(new IllegalStateException(message))
