package org.updraft0.controltower.sdeloader

import org.updraft0.controltower.db.model.*
import org.updraft0.controltower.db.query
import org.updraft0.controltower.sde
import org.updraft0.controltower.sde.ExportedData
import org.updraft0.controltower.constant.SystemId
import zio.*
import zio.stream.ZStream

import java.time.Instant
import javax.sql.DataSource

private[sdeloader] val Language = "en"

// after the drifter conflict, the SDE contains the new names of the systems
private[sdeloader] val SystemNameOverrides = Map(
  SystemId(31000001) -> "J055520",
  SystemId(31000002) -> "J110145",
  SystemId(31000003) -> "J164710",
  SystemId(31000004) -> "J200727",
  SystemId(31000006) -> "J174618"
)

/** Loads the whole of the SDE (group) data into the db
  */
def intoDb(
    sdeEntries: ZStream[Any, sde.parser.Error, ExportedData],
    releasedAt: Instant,
    buildNumber: Long,
    meta: SdeLoadMeta
): RIO[DataSource, (Int, Chunk[Long])] =
  for
    _       <- ZIO.logDebug(s"Loading build $buildNumber released at $releasedAt into SDE tables")
    version <- query.sde.insertVersion(releasedAt, buildNumber, meta)
    res     <- sdeEntries
      .mapError(toThrowable)
      .mapChunksZIO(ed => loadChunkOfData(ed))
      .runCollect
    _ <- backfillEntries
  yield (version, res)

private def toThrowable(e: sde.parser.Error): Throwable =
  e match
    case sde.parser.Error.Zip(sde.zip.Error.IOError(ioe)) => ioe
    case sde.parser.Error.Zip(sde.zip.Error.Unknown(u))   => u
    case sde.parser.Error.Json(j)                         => j
    case sde.parser.Error.Unknown(u)                      => u

private[sdeloader] def loadChunkOfData(chunk: Chunk[ExportedData]): RIO[DataSource, Chunk[Long]] =
  // want to load same type of 'thing', don't want to rely on internal stream chunking to provide this guarantee
  if chunk.isEmpty then ZIO.logWarning("empty chunk received, maybe not normal?").as(Chunk.empty)
  else if chunk.forall(_.ordinal == chunk.head.ordinal) then loadChunkSameData(chunk)
  else
    ZIO
      .foreach(chunk.groupBy(_.ordinal))((k, v) => loadChunkSameData(v).map(c => k -> c))
      .map(_.values.fold(Chunk.empty)(_ ++ _))

private[sdeloader] def loadChunkSameData(data: Chunk[ExportedData]): RIO[DataSource, Chunk[Long]] =
  data.head match
    // single-entry chunks
    case ExportedData.CategoryIds(cs) =>
      query.sde.insertItemCategories(cs.map(toItemCategory)).map(Chunk(_))
    case ExportedData.Constellations(cs) =>
      query.sde.insertConstellations(cs.map(toConstellation)).map(Chunk(_))
    case ExportedData.DogmaAttributeCategories(dacs) =>
      query.sde.insertDogmaAttributeCategories(dacs.map(toDogmaAttributeCategory)).map(Chunk(_))
    case ExportedData.DogmaAttributes(das) =>
      query.sde.insertDogmaAttributeTypes(das.map(toDogmaAttributeType)).map(Chunk(_))
    case ExportedData.Factions(fs) =>
      query.sde.insertFactions(fs.map(toFaction)).map(Chunk(_))
    case ExportedData.GroupIds(gs) =>
      query.sde.insertItemGroups(gs.map(toItemGroup)).map(Chunk(_))
    case ExportedData.NpcCorporations(ncs) =>
      query.sde.insertNpcCorporations(ncs.map(toNpcCorporation)).map(Chunk(_))
    case ExportedData.NpcStations(nss) =>
      query.sde.insertNpcStations(nss.map(toNpcStation)).map(Chunk(_))
    case ExportedData.Regions(rs) =>
      query.sde.insertRegions(rs.map(toRegion)).map(Chunk(_))
    case ExportedData.SecondarySuns(sss) =>
      query.sde.insertSolarSystemEffects(sss.map(toSolarSystemEffect)).map(Chunk(_))
    case ExportedData.StationServices(sss) =>
      query.sde.insertStationServices(sss.map(toStationService)).map(Chunk(_))
    case ExportedData.StationOperations(sos) =>
      loadStationOperations(sos).map(Chunk(_))
    case ExportedData.TypeDogmas(tds) =>
      loadTypeDogmas(tds).map(Chunk(_))
    // multi-entry chunks
    case _: ExportedData.SolarSystem =>
      query.sde.insertSolarSystems(data.map(_.asInstanceOf[ExportedData.SolarSystem]).map(toSolarSystem)).map(Chunk(_))
    case _: ExportedData.Planet =>
      query.sde.insertSolarSystemPlanets(data.map(_.asInstanceOf[ExportedData.Planet]).map(toPlanet)).map(Chunk(_))
    case _: ExportedData.Stargate =>
      query.sde.insertStargates(data.map(_.asInstanceOf[ExportedData.Stargate]).map(toStargate)).map(Chunk(_))
    case _: ExportedData.Star =>
      query.sde.insertSolarSystemStars(data.map(_.asInstanceOf[ExportedData.Star]).map(toStar)).map(Chunk(_))
    case _: ExportedData.TypeId =>
      query.sde.insertItemTypes(data.map(_.asInstanceOf[ExportedData.TypeId]).map(toItemType)).map(Chunk(_))

private def backfillEntries =
  query.sde.updateSolarSystemColumns
    <*> query.sde.updateSolarSystemWhClassFromRegion
    <*> query.sde.updateConstellationRegionName
    <*> query.sde.updateNpcStationName

private[sdeloader] def loadTypeDogmas(attrs: Chunk[sde.TypeDogma]) =
  query.sde.insertItemDogmaAttributes(attrs.flatMap(toItemDogmaAttributes))

private def toItemDogmaAttributes(t: sde.TypeDogma): Vector[ItemDogmaAttribute] =
  t.attributes.map(p => ItemDogmaAttribute(t.id, p.id, p.value))

private[sdeloader] def loadStationOperations(operations: Chunk[sde.StationOperation]) =
  for
    oc  <- query.sde.insertStationOperations(operations.map(toStationOperation))
    osc <- query.sde.insertStationOperationServices(
      operations.flatMap(so => so.services.map(sid => StationOperationService(so.id, sid)))
    )
  yield oc + osc

private[sdeloader] def toSolarSystem(ss: ExportedData.SolarSystem) =
  // empty entries here will be backfilled after insert
  SolarSystem(
    id = ss.id,
    starId = ss.starId,
    name = SystemNameOverrides.getOrElse(ss.id, ss.nameMap(Language)),
    regionName = "",
    regionId = ss.regionId,
    constellationName = "",
    constellationId = ss.constellationId,
    effectTypeId = None,
    whClassId = ss.whClassId,
    security = ss.securityStatus
  )

private def toRegion(r: sde.Region) =
  Region(
    id = r.id,
    name = r.nameMap(Language),
    whClassId = r.wormholeClassId.map(_.value),
    factionId = r.factionId
  )

private def toConstellation(
    c: sde.Constellation
) =
  Constellation(
    id = c.id,
    name = c.nameMap(Language),
    regionId = c.regionId,
    regionName = ""
  )

private def toPlanet(p: ExportedData.Planet) =
  SolarSystemPlanet(p.id, p.solarSystemId, p.celestialIndex, p.typeId, p.moonIds.fold(0)(_.size))

private def toStargate(sg: sde.ExportedData.Stargate) =
  Stargate(
    id = sg.id,
    systemId = sg.solarSystemId,
    toSystemId = sg.destination.solarSystemId,
    toStargateId = sg.destination.stargateId
  )

private def toStar(s: sde.ExportedData.Star) =
  SolarSystemStar(
    id = s.id,
    systemId = s.solarSystemId,
    typeId = s.typeId
  )

private def toFaction(f: sde.Faction): Faction =
  Faction(
    id = f.id,
    name = f.nameMap(Language),
    corporationId = f.corporationId.map(_.value),
    iconId = f.iconId,
    militiaCorporationId = f.militiaCorporationId.map(_.value),
    sizeFactor = f.sizeFactor,
    systemId = f.solarSystemId,
    uniqueName = f.uniqueName
  )
private def toNpcStation(
    ns: sde.NpcStation
) =
  NpcStation(
    id = ns.id,
    name = "",
    ownerId = ns.ownerId,
    typeId = ns.typeId.value,
    operationId = ns.operationId,
    orbitId = ns.orbitId,
    celestialIndex = ns.celestialIndex,
    orbitIndex = ns.orbitIndex,
    systemId = ns.systemId
  )

private def toNpcCorporation(c: sde.NpcCorporation): NpcCorporation =
  NpcCorporation(
    id = c.id,
    name = c.nameMap(Language),
    ceoId = c.ceoId,
    raceId = c.raceId,
    factionId = c.factionId,
    iconId = c.iconId,
    solarSystemId = c.solarSystemId.map(_.value),
    stationId = c.stationId,
    ticker = c.ticker,
    uniqueName = c.uniqueName
  )

private def toSolarSystemEffect(ss: sde.SecondarySun): SolarSystemEffect =
  SolarSystemEffect(
    id = ss.id,
    systemId = ss.solarSystemId,
    typeId = ss.effectId.typeId
  )

private def toDogmaAttributeCategory(c: sde.DogmaAttributeCategory) =
  DogmaAttributeCategory(c.id, c.name, c.description)

private def toDogmaAttributeType(a: sde.DogmaAttribute) =
  DogmaAttributeType(a.id, a.categoryId, a.dataType, a.name, a.description, a.defaultValue, a.unitId, a.iconId)

private def toItemCategory(ci: sde.CategoryId): ItemCategory = ItemCategory(ci.id, ci.nameMap(Language), ci.iconId)
private def toItemGroup(gi: sde.GroupId): ItemGroup = ItemGroup(gi.id, gi.categoryId, gi.nameMap(Language), gi.iconId)
private def toItemType(ti: sde.ExportedData.TypeId): ItemType =
  ItemType(
    id = ti.id,
    name = ti.nameMap(Language),
    groupId = ti.groupId,
    mass = ti.mass,
    volume = ti.volume
  )
private def toStationOperation(so: sde.StationOperation): StationOperation =
  StationOperation(so.id, so.activityId, so.nameMap(Language))

private def toStationService(ss: sde.StationService): StationService = StationService(ss.id, ss.nameMap(Language))
