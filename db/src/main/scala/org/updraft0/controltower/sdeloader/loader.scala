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
  case ExportedData.CategoryIds(categoryIds) =>
    query.sde.insertItemCategories(categoryIds.map(toItemCategory))
  case ExportedData.DogmaAttributeCategories(categories) =>
    query.sde.insertDogmaAttributeCategories(categories.map(toDogmaAttributeCategory))
  case ExportedData.DogmaAttributes(attributes) =>
    query.sde.insertDogmaAttributeTypes(attributes.map(toDogmaAttributeType))
  case ExportedData.Factions(factions)            => query.sde.insertFactions(factions.map(toFaction))
  case ExportedData.GroupIds(groupIds)            => query.sde.insertItemGroups(groupIds.map(toItemGroup))
  case ExportedData.NpcCorporations(corporations) => query.sde.insertNpcCorporations(corporations.map(toNpcCorporation))
  case ExportedData.StationOperations(stationOperations) => loadStationOperations(stationOperations)
  case ExportedData.StationServices(stationServices) =>
    query.sde.insertStationServices(stationServices.map(toStationService))
  case ExportedData.TypeDogmas(dogmas) => loadTypeDogmas(dogmas)
  case ExportedData.TypeIds(ids)       => query.sde.insertItemTypes(ids.map(toItemType))
  case ExportedData.UniqueNames(names) => query.sde.insertItemNames(names.map(toItemName))
  // unsupported export types
  case _: ExportedData.Region | _: ExportedData.Constellation | _: ExportedData.SolarSystem =>
    unsupported("BUG: cannot import a single region/constellation/solar system standalone")
}

private[sdeloader] def loadTypeDogmas(attrs: Vector[sde.TypeDogma]) =
  query.sde.insertItemDogmaAttributes(attrs.flatMap(toItemDogmaAttributes))

private[sdeloader] def loadSolarSystems(s: ImportState.ReadyForSolarSystems, rss: GroupedExport.RegionSolarSystems) =
  for
    rc <- query.sde.upsertRegion(toRegion(s.uniqueNamesById, rss.region))
    cc <- query.sde.insertConstellation(toConstellation(s.uniqueNamesById, rss.region, rss.constellation))
    sc <- ZIO
      .foreach(rss.solarSystems)(insertSolarSystem(s.uniqueNamesById, rss.region, rss.constellation, _))
      .map(_.sum)
  yield rc + cc + sc

private[sdeloader] def loadStationOperations(operations: Vector[sde.StationOperation]) =
  for
    oc <- query.sde.insertStationOperations(operations.map(toStationOperation))
    osc <- query.sde.insertStationOperationServices(
      operations.flatMap(so => so.services.map(sid => StationOperationService(so.id, sid)))
    )
  yield oc + osc

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

private def insertSolarSystemMoons(s: ExportedData.SolarSystem) =
  ZIO
    .foreach(s.planets.flatMap(p => p.moons.zipWithIndex.map((planetMoon, moonIdx) => (p, planetMoon, moonIdx))))(
      (planet, planetMoon, moonIdx) => query.sde.insertSolarSystemMoon(toPlanetMoon(s, planet, planetMoon, moonIdx + 1))
    )
    .map(_.sum)

private def insertNpcStations(names: Map[Long, sde.UniqueName], s: ExportedData.SolarSystem) =
  ZIO
    .foreach(
      s.planets.flatMap(p =>
        p.moons.flatMap(m => m.npcStations.map(ns => (p, Some(m), ns))) ++
          p.stations.map(ns => (p, None, ns))
      )
    )((p, m, ns) => query.sde.insertNpcStation(toNpcStation(names, s, p, m, ns)))
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
private def toFaction(f: sde.Faction): Faction =
  Faction(
    id = f.id,
    name = f.nameEn,
    corporationId = f.corporationId,
    description = f.descriptionEn,
    shortDescription = f.shortDescriptionEn,
    iconId = f.iconId,
    militiaCorporationId = f.militiaCorporationId,
    sizeFactor = f.sizeFactor,
    systemId = f.solarSystemId,
    uniqueName = f.uniqueName
  )
private def toNpcStation(
    names: Map[Long, sde.UniqueName],
    s: ExportedData.SolarSystem,
    p: sde.Planet,
    m: Option[sde.PlanetMoon],
    ns: sde.NpcStation
) =
  NpcStation(
    id = ns.id,
    name = names(ns.id).name,
    ownerId = ns.ownerId,
    typeId = ns.typeId,
    operationId = ns.operationId,
    planetId = p.id,
    moonId = m.map(_.id),
    systemId = s.id
  )

private def toNpcCorporation(c: sde.NpcCorporation): NpcCorporation =
  NpcCorporation(
    id = c.id,
    name = c.nameEn,
    ceoId = c.ceoId,
    description = c.descriptionEn,
    raceId = c.raceId,
    factionId = c.factionId,
    iconId = c.iconId,
    solarSystemId = c.solarSystemId,
    stationId = c.stationId,
    ticker = c.ticker,
    uniqueName = c.uniqueName
  )

private def toItemDogmaAttributes(t: sde.TypeDogma): Vector[ItemDogmaAttribute] =
  t.attributes.map((attrId, value) => ItemDogmaAttribute(t.id, attrId, value)).toVector

private def toDogmaAttributeCategory(c: sde.DogmaAttributeCategory) =
  DogmaAttributeCategory(c.id, c.name, c.description)

private def toDogmaAttributeType(a: sde.DogmaAttribute) =
  DogmaAttributeType(a.id, a.categoryId, a.dataType, a.name, a.description, a.defaultValue, a.unitId, a.iconId)

private def toItemCategory(ci: sde.CategoryId): ItemCategory = ItemCategory(ci.id, ci.nameEn, ci.iconId)
private def toItemGroup(gi: sde.GroupId): ItemGroup          = ItemGroup(gi.id, gi.categoryId, gi.nameEn, gi.iconId)
private def toItemName(un: sde.UniqueName): ItemName         = ItemName(un.itemId, un.groupId, un.name)
private def toItemType(ti: sde.TypeId): ItemType =
  ItemType(
    id = ti.id,
    name = ti.nameEn,
    groupId = ti.groupId,
    description = ti.descriptionEn,
    mass = ti.mass,
    volume = ti.volume
  )
private def toStationOperation(so: sde.StationOperation): StationOperation =
  StationOperation(so.id, so.activityId, so.nameEn, so.descriptionEn)
private def toStationService(ss: sde.StationService): StationService = StationService(ss.id, ss.nameEn)

private def unsupported(message: String): RIO[Any, Nothing] = ZIO.fail(new IllegalStateException(message))
