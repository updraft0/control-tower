package org.updraft0.controltower.sde

import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import org.updraft0.controltower.sde.yaml.{Cursor, KeyType, YAML, YamlArray, YamlObject, YamlValue, given}
import zio.*

/** Parser of SDE data export types
  */
object parser:

  private val RegionData        = """sde/fsd/universe/\w+/([-\w]+)/region.staticdata""".r
  private val ConstellationData = """sde/fsd/universe/\w+/([-\w]+)/([-\w]+)/constellation.staticdata""".r
  private val SolarSystemData   = """sde/fsd/universe/\w+/([-\w]+)/([-\w]+)/([-\w]+)/solarsystem.staticdata""".r

  private type Parser[T] = IO[Error, T]

  enum Error:
    case Yaml(message: String, id: String, cause: yaml.Error)
    case Zip(error: zip.Error)
    case Unknown(cause: Throwable)

  def parse(entry: zip.ZipEntry): ZIO[LoadSettings, Error, Option[ExportedData]] = {
    val bytes = entry.bytes
    ZIO.logTrace(s"parsing entry ${entry.name}") *> (entry.name match {
      case "sde/bsd/invUniqueNames.yaml" =>
        parseYamlArray(bytes).flatMap(parseUniqueNames).asSome
      case "sde/fsd/categoryIDs.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseCategoryIds).asSome
      case "sde/fsd/dogmaAttributeCategories.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseDogmaAttributeCategories).asSome
      case "sde/fsd/dogmaAttributes.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseDogmaAttributes).asSome
      case "sde/fsd/factions.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseFactions).asSome
      case "sde/fsd/npcCorporations.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseNpcCorporations).asSome
      case "sde/fsd/groupIDs.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseGroupIds).asSome
      case "sde/fsd/stationOperations.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseStationOperations).asSome
      case "sde/fsd/stationServices.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseStationServices).asSome
      case "sde/fsd/typeDogma.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseTypeDogmas).asSome
      case "sde/fsd/typeIDs.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseTypeIds).asSome
      case RegionData(regionName) =>
        parseYaml[String](bytes).flatMap(parseRegion(regionName, _)).asSome
      case ConstellationData(region, constellation) =>
        parseYaml[String](bytes).flatMap(parseConstellation(region, constellation, _)).asSome
      case SolarSystemData(region, constellation, name) =>
        parseYaml[String](bytes).flatMap(parseSolarSystem(region, constellation, name, _)).asSome
      case _ => ZIO.logWarning(s"Ignoring unmapped SDE entry ${entry.name}") *> ZIO.succeed(None)
    })
  }

  private[sde] def parseDogmaAttributeCategories(
      yaml: YamlObject[Integer]
  ): Parser[ExportedData.DogmaAttributeCategories] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseDogmaAttributeCategory))
      .mapBoth(
        Error.Yaml("Failed to parse DogmaAttributeCategories", "", _),
        ExportedData.DogmaAttributeCategories.apply
      )

  private def parseDogmaAttributeCategory(id: Int, c: Cursor[String]): YamlValue[DogmaAttributeCategory] =
    for
      name        <- c.downField("name").as[String]
      description <- c.downField("description").as[Option[String]]
    yield DogmaAttributeCategory(id, name, description)

  private[sde] def parseDogmaAttributes(yaml: YamlObject[Integer]): Parser[ExportedData.DogmaAttributes] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseDogmaAttribute))
      .mapBoth(
        Error.Yaml("Failed to parse DogmaAttributes", "", _),
        ExportedData.DogmaAttributes.apply
      )

  private def parseDogmaAttribute(id: Int, c: Cursor[String]): YamlValue[DogmaAttribute] =
    for
      categoryId   <- c.downField("categoryID").as[Option[Long]]
      dataType     <- c.downField("dataType").as[Int]
      name         <- c.downField("name").as[String]
      description  <- c.downField("description").as[Option[String]]
      defaultValue <- c.downField("defaultValue").as[Double]
      unitId       <- c.downField("unitID").as[Option[Int]]
      iconId       <- c.downField("iconID").as[Option[Long]]
    yield DogmaAttribute(id.toLong, categoryId, dataType, name, description, defaultValue, unitId, iconId)

  private[sde] def parseCategoryIds(yaml: YamlObject[Integer]): Parser[ExportedData.CategoryIds] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseCategoryId))
      .mapBoth(Error.Yaml("Failed to parse CategoryIds", "", _), ExportedData.CategoryIds.apply)

  private def parseCategoryId(id: Int, c: Cursor[String]): YamlValue[CategoryId] =
    for
      nameEn <- c.downField("name").downField("en").as[String]
      iconId <- c.downField("iconID").as[Option[Long]]
    yield CategoryId(id, nameEn, iconId)

  private[sde] def parseFactions(yaml: YamlObject[Integer]): Parser[ExportedData.Factions] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseFaction))
      .mapBoth(Error.Yaml("Failed to parse Factions", "", _), ExportedData.Factions.apply)

  private def parseFaction(id: Int, c: Cursor[String]): YamlValue[Faction] =
    for
      nameEn               <- c.downField("nameID").downField("en").as[String]
      corporationId        <- c.downField("corporationID").as[Option[Long]]
      descriptionEn        <- c.downField("descriptionID").downField("en").as[String]
      shortDescriptionEn   <- c.downField("shortDescriptionID").downField("en").as[Option[String]]
      iconId               <- c.downField("iconID").as[Long]
      militiaCorporationId <- c.downField("militiaCorporationID").as[Option[Long]]
      memberRaces          <- c.downField("memberRaces").as[Vector[Int]]
      sizeFactor           <- c.downField("sizeFactor").as[Double]
      solarSystemId        <- c.downField("solarSystemID").as[Long]
      uniqueName           <- c.downField("uniqueName").as[Boolean]
    yield Faction(
      id = id.toLong,
      nameEn = nameEn,
      corporationId = corporationId,
      descriptionEn = descriptionEn,
      shortDescriptionEn = shortDescriptionEn,
      iconId = iconId,
      militiaCorporationId = militiaCorporationId,
      memberRaces = memberRaces,
      sizeFactor = sizeFactor,
      solarSystemId = solarSystemId,
      uniqueName = uniqueName
    )

  private[sde] def parseGroupIds(yaml: YamlObject[Integer]): Parser[ExportedData.GroupIds] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseGroupId))
      .mapBoth(Error.Yaml("Failed to parse GroupIds", "", _), ExportedData.GroupIds.apply)

  private def parseGroupId(id: Int, c: Cursor[String]): YamlValue[GroupId] =
    for
      categoryId <- c.downField("categoryID").as[Long]
      nameEn     <- c.downField("name").downField("en").as[String]
      iconId     <- c.downField("iconID").as[Option[Long]]
    yield GroupId(id, categoryId, nameEn, iconId)

  private[sde] def parseNpcCorporations(yaml: YamlObject[Integer]): Parser[ExportedData.NpcCorporations] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseNpcCorporation))
      .mapBoth(Error.Yaml("Failed to parse NpcCorporations", "", _), ExportedData.NpcCorporations.apply)

  private def parseNpcCorporation(id: Int, c: Cursor[String]): YamlValue[NpcCorporation] =
    for
      nameEn        <- c.downField("nameID").downField("en").as[String]
      allowedRaces  <- c.downField("allowedMemberRaces").as[Option[Vector[Long]]]
      ceoId         <- c.downField("ceoID").as[Option[Long]]
      raceId        <- c.downField("raceID").as[Option[Int]]
      descriptionEn <- c.downField("descriptionID").downField("en").as[Option[String]]
      factionId     <- c.downField("factionID").as[Option[Long]]
      iconId        <- c.downField("iconID").as[Option[Long]]
      solarSystemId <- c.downField("solarSystemID").as[Option[Long]]
      stationId     <- c.downField("stationID").as[Option[Long]]
      ticker        <- c.downField("tickerName").as[String]
      uniqueName    <- c.downField("uniqueName").as[Boolean]
    yield NpcCorporation(
      id = id.toLong,
      nameEn = nameEn,
      allowedRaces = allowedRaces,
      ceoId = ceoId,
      raceId = raceId,
      descriptionEn = descriptionEn,
      factionId = factionId,
      iconId = iconId,
      solarSystemId = solarSystemId,
      stationId = stationId,
      ticker = ticker,
      uniqueName = uniqueName
    )

  private[sde] def parseUniqueNames(yaml: YamlArray): Parser[ExportedData.UniqueNames] =
    YAML
      .mapArrayCursor(yaml, parseUniqueName)
      .mapBoth(Error.Yaml("Failed to parse InvUniqueNames", "", _), ExportedData.UniqueNames.apply)

  private def parseUniqueName(c: Cursor[String]): YamlValue[UniqueName] =
    for
      id      <- c.downField("itemID").as[Long]
      groupId <- c.downField("groupID").as[Int]
      name    <- c.downField("itemName").as[String]
    yield UniqueName(id, groupId, name)

  private[sde] def parseStationOperations(yaml: YamlObject[Integer]): Parser[ExportedData] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseStationOperation))
      .mapBoth(Error.Yaml("Failed to parse StationOperations", "", _), ExportedData.StationOperations.apply)

  private def parseStationOperation(id: Int, c: Cursor[String]): YamlValue[StationOperation] =
    for
      activityId    <- c.downField("activityID").as[Int]
      nameEn        <- c.downField("operationNameID").downField("en").as[String]
      descriptionEn <- c.downField("descriptionID").downField("en").as[Option[String]]
      services      <- c.downField("services").as[Vector[Int]]
      stationTypes  <- c.downField("stationTypes").as[Map[Int, Int]]
    yield StationOperation(
      id = id.toLong,
      activityId = activityId,
      nameEn = nameEn,
      descriptionEn = descriptionEn,
      services = services,
      stationTypes = stationTypes
    )

  private[sde] def parseStationServices(yaml: YamlObject[Integer]): Parser[ExportedData] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseStationService))
      .mapBoth(Error.Yaml("Failed to parse StationServices", "", _), ExportedData.StationServices.apply)

  private def parseStationService(id: Int, c: Cursor[String]): YamlValue[StationService] =
    for nameEn <- c.downField("serviceNameID").downField("en").as[String]
    yield StationService(id, nameEn)

  private[sde] def parseTypeDogmas(yaml: YamlObject[Integer]): Parser[ExportedData] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseTypeDogma))
      .mapBoth(Error.Yaml("Failed to parse TypeDogmas", "", _), ExportedData.TypeDogmas.apply)

  private def parseTypeDogma(id: Int, c: Cursor[String]): YamlValue[TypeDogma] =
    for
      attributes <- c.downField("dogmaAttributes").mapArray(parseDogmaAttribute).map(_.toMap)
      effects    <- c.downField("dogmaEffects").mapArray(parseDogmaEffect).map(_.toMap)
    yield TypeDogma(id, attributes, effects)

  private def parseDogmaAttribute(c: Cursor[String]): YamlValue[(Long, Double)] =
    for
      attributeId <- c.downField("attributeID").as[Long]
      value       <- c.downField("value").as[Double]
    yield attributeId -> value

  private def parseDogmaEffect(c: Cursor[String]): YamlValue[(Long, Boolean)] =
    for
      effectId  <- c.downField("effectID").as[Long]
      isDefault <- c.downField("isDefault").as[Boolean]
    yield effectId -> isDefault

  private[sde] def parseTypeIds(yaml: YamlObject[Integer]): Parser[ExportedData] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseTypeId))
      .mapBoth(Error.Yaml("Failed to parse TypeIds", "", _), ExportedData.TypeIds.apply)

  private def parseTypeId(id: Int, c: Cursor[String]): YamlValue[TypeId] =
    for
      descriptionEn <- c.downField("description").downField("en").as[Option[String]]
      nameEn        <- c.downField("name").downField("en").as[String]
      groupId       <- c.downField("groupID").as[Long]
    yield TypeId(id.toLong, nameEn, groupId, descriptionEn)

  private[sde] def parseRegion(tag: String, yaml: YamlObject[String]): Parser[ExportedData] =
    (for
      c         <- YAML.cursor(yaml)
      id        <- c.downField("regionID").as[Long]
      nameId    <- c.downField("nameID").as[Long]
      whClass   <- c.downField("wormholeClassID").as[Option[Int]].map(_.map(whClassFromId))
      factionId <- c.downField("factionID").as[Option[Long]]
    yield ExportedData.Region(id, nameId, tag, whClass, factionId))
      .mapError(Error.Yaml("Failed to parse Region", tag, _))

  private[sde] def parseConstellation(
      regionTag: String,
      tag: String,
      yaml: YamlObject[String]
  ): Parser[ExportedData] =
    (for
      c      <- YAML.cursor(yaml)
      id     <- c.downField("constellationID").as[Long]
      nameId <- c.downField("nameID").as[Long]
    yield ExportedData.Constellation(id, nameId, tag, regionTag))
      .mapError(Error.Yaml("Failed to parse Constellation", tag, _))

  private[sde] def parseSolarSystem(
      regionTag: String,
      constellationTag: String,
      tag: String,
      yaml: YamlObject[String]
  ): Parser[ExportedData] =
    (for
      c              <- YAML.cursor(yaml)
      id             <- c.downField("solarSystemID").as[Long]
      nameId         <- c.downField("solarSystemNameID").as[Long]
      star           <- c.downField("star").mapOptional(parseStar)
      wormholeEffect <- c.downField("secondarySun").downField("typeID").as[Option[Long]].map(toWhEffect)
      planets        <- parsePlanets(c.downField("planets"))
      stargates      <- parseStargates(c.downField("stargates"))
      securityClass  <- c.downField("securityClass").as[Option[String]]
      security       <- c.downField("security").as[Option[Double]]
      border         <- c.downField("border").as[Boolean]
      corridor       <- c.downField("corridor").as[Boolean]
      fringe         <- c.downField("fringe").as[Boolean]
      hub            <- c.downField("hub").as[Boolean]
      international  <- c.downField("international").as[Boolean]
      regional       <- c.downField("regional").as[Boolean]
    yield ExportedData.SolarSystem(
      tag = tag,
      constellationTag = constellationTag,
      regionTag = regionTag,
      id = id,
      nameId = nameId,
      star = star,
      secondaryEffect = wormholeEffect,
      planets = planets,
      stargates = stargates,
      securityClass = securityClass,
      security = security,
      border = border,
      corridor = corridor,
      fringe = fringe,
      hub = hub,
      international = international,
      regional = regional
    )).mapError(Error.Yaml("Failed to parse SolarSystem", tag, _))

  private def parseStar(c: Cursor[String]): YamlValue[Star] =
    for
      id     <- c.downField("id").as[Long]
      typeId <- c.downField("typeID").as[Long]
    yield Star(id, typeId)

  private def parseStargates(c: Cursor[String]): YamlValue[Vector[Stargate]] =
    parseIntKeyedMap(c, parseStargate).map(_.toVector)

  private def parseStargate(id: Int, c: Cursor[String]): YamlValue[Stargate] =
    for destinationId <- c.downField("destination").as[Long]
    yield Stargate(id.toLong, destinationId)

  private def parsePlanets(c: Cursor[String]): YamlValue[Vector[Planet]] =
    parseIntKeyedMap(c, parsePlanet).map(_.sortBy(_.index))

  private def parsePlanet(id: Int, c: Cursor[String]): YamlValue[Planet] =
    for
      index         <- c.downField("celestialIndex").as[Int]
      typeId        <- c.downField("typeID").as[Long]
      moons         <- parseMoons(c.downField("moons"))
      asteroidBelts <- parseAsteroidBelts(c.downField("asteroidBelts"))
    yield Planet(id.toLong, index, typeId, moons, asteroidBelts)

  private def parseMoons(c: Cursor[String]): YamlValue[Vector[PlanetMoon]] =
    parseIntKeyedMap(c, parseMoon)

  private def parseMoon(id: Int, c: Cursor[String]): YamlValue[PlanetMoon] =
    for stations <- parseNpcStations(c.downField("npcStations"))
    yield PlanetMoon(id.toLong, stations)

  private def parseNpcStations(c: Cursor[String]): YamlValue[Vector[NpcStation]] =
    parseIntKeyedMap(c, parseNpcStation)

  private def parseNpcStation(id: Int, c: Cursor[String]): YamlValue[NpcStation] =
    for
      ownerId     <- c.downField("ownerID").as[Long]
      typeId      <- c.downField("typeID").as[Long]
      operationId <- c.downField("operationID").as[Long]
    yield NpcStation(id.toLong, ownerId, typeId, operationId)

  private def parseAsteroidBelts(c: Cursor[String]): YamlValue[Vector[PlanetAsteroidBelt]] =
    parseIntKeyedMap(c, (id, _) => ZIO.succeed(PlanetAsteroidBelt(id.toLong)))

  private def whClassFromId(id: Int): WormholeClass =
    WormholeClass.values.find(_.value == id).getOrElse(throw new IllegalArgumentException(s"${id} not a wh class"))

  private def toWhEffect(idOpt: Option[Long]): Option[WormholeEffect] =
    idOpt.map(id =>
      WormholeEffect.values
        .find(_.typeId == id)
        .getOrElse(throw new IllegalArgumentException(s"${id} is not a wh effect"))
    )

  private def parseIntKeyedMap[T](
      c: Cursor[_ <: KeyType],
      f: (Int, Cursor[String]) => YamlValue[T]
  ): YamlValue[Vector[T]] =
    c.mapObject[Integer, String, T]((id, mc) => f(id, mc)).map(_.map(_._2).toVector)

  private[sde] def parseYaml[K <: KeyType](bytes: Array[Byte]): ZIO[LoadSettings, Error, YamlObject[K]] =
    YAML.parse(bytes).mapError(Error.Yaml("Invalid YAML", "?", _))

  private[sde] def parseYaml[K <: KeyType](s: String): ZIO[LoadSettings, Error, YamlObject[K]] =
    YAML.parse(s).mapError(Error.Yaml("Invalid YAML", "?", _))

  private[sde] def parseYamlArray(bytes: Array[Byte]): ZIO[LoadSettings, Error, YamlArray] =
    YAML.parseArray(bytes).mapError(Error.Yaml("Invalid YAML Array", "?", _))

  private[sde] def parseYamlArray(s: String): ZIO[LoadSettings, Error, YamlArray] =
    YAML.parseArray(s).mapError(Error.Yaml("Invalid YAML Array", "?", _))
