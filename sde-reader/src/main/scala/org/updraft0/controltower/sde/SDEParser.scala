package org.updraft0.controltower.sde

import zio.*
import org.snakeyaml.engine.v2.api.Load
import org.updraft0.controltower.sde.yaml.{KeyType, YamlArray, YamlObject, Cursor, YAML, YamlValue, given}

/** Parser of SDE data export types
  */
object SDEParser:

  private val RegionData        = """sde/fsd/universe/\w+/([-\w]+)/region.staticdata""".r
  private val ConstellationData = """sde/fsd/universe/\w+/([-\w]+)/([-\w]+)/constellation.staticdata""".r
  private val SolarSystemData   = """sde/fsd/universe/\w+/([-\w]+)/([-\w]+)/([-\w]+)/solarsystem.staticdata""".r

  private type Parser[T] = IO[Error, T]

  enum Error:
    case Yaml(message: String, id: String, cause: yaml.Error)
    case Unknown(cause: Throwable)

  def parse(entry: zip.ZipEntry): ZIO[Load, Error, Option[DomainTopLevel]] = {
    val bytes = entry.bytes
    entry.name match {
      case "sde/bsd/invNames.yaml" =>
        parseYamlArray(bytes).flatMap(parseItemNames).asSome
      case "sde/fsd/stationServices.yaml" =>
        parseYaml[Integer](bytes).flatMap(parseStationServices).asSome
      case RegionData(regionName) =>
        parseYaml[String](bytes).flatMap(parseRegion(regionName, _)).asSome
      case ConstellationData(region, constellation) =>
        parseYaml[String](bytes).flatMap(parseConstellation(region, constellation, _)).asSome
      case SolarSystemData(region, constellation, name) =>
        parseYaml[String](bytes).flatMap(parseSolarSystem(region, constellation, name, _)).asSome
      case _ => ZIO.succeed(None)
    }
  }

  private[sde] def parseItemNames(yaml: YamlArray): Parser[Vector[ItemName]] =
    YAML.mapArrayCursor(yaml, parseItemName).mapError(Error.Yaml("Failed to parse InvItems", "", _))

  private def parseItemName(c: Cursor[String]): YamlValue[ItemName] =
    for
      id   <- c.downField("itemID").as[Long]
      name <- c.downField("itemName").as[String]
    yield ItemName(id, name)

  private[sde] def parseStationServices(yaml: YamlObject[Integer]): Parser[Vector[StationService]] =
    YAML
      .cursor[Integer](yaml)
      .flatMap(parseIntKeyedMap(_, parseStationService))
      .mapError(Error.Yaml("Failed to parse StationServices", "", _))

  private def parseStationService(id: Int, c: Cursor[String]): YamlValue[StationService] =
    for nameEn <- c.downField("serviceNameID").downField("en").as[String]
    yield StationService(id, nameEn)

  private[sde] def parseRegion(name: String, yaml: YamlObject[String]): Parser[Region] =
    (for
      c         <- YAML.cursor(yaml)
      id        <- c.downField("regionID").as[Long]
      nameId    <- c.downField("nameID").as[Long]
      whClass   <- c.downField("wormholeClassID").as[Option[Int]].map(_.map(whClassFromId))
      factionId <- c.downField("factionID").as[Option[Long]]
    yield Region(id, nameId, name, whClass, factionId))
      .mapError(Error.Yaml("Failed to parse Region", name, _))

  private[sde] def parseConstellation(region: String, name: String, yaml: YamlObject[String]): Parser[Constellation] =
    (for
      c      <- YAML.cursor(yaml)
      id     <- c.downField("constellationID").as[Long]
      nameId <- c.downField("nameID").as[Long]
    yield Constellation(id, nameId, name, region))
      .mapError(Error.Yaml("Failed to parse Constellation", name, _))

  private[sde] def parseSolarSystem(
      region: String,
      constellation: String,
      name: String,
      yaml: YamlObject[String]
  ): Parser[SolarSystem] =
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
    yield SolarSystem(
      name = name,
      constellation = constellation,
      region = region,
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
    )).mapError(Error.Yaml("Failed to parse SolarSystem", name, _))

  private def parseStar(c: Cursor[String]): YamlValue[Star] =
    for
      id     <- c.downField("id").as[Long]
      typeId <- c.downField("typeID").as[Long]
    yield Star(id, typeId)

  private def parseStargates(c: Cursor[String]): YamlValue[Vector[Stargate]] =
    parseIntKeyedMap(c, parseStargate)

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
      ownerId <- c.downField("ownerID").as[Long]
      typeId  <- c.downField("typeID").as[Long]
    yield NpcStation(id.toLong, ownerId, typeId)

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
    c.mapObject[Integer, String, T]((id, mc) => f(id, mc)).map(_.values.toVector)

  private[sde] def parseYaml[K <: KeyType](bytes: Array[Byte]): ZIO[Load, Error, YamlObject[K]] =
    YAML.parse(bytes).mapError(Error.Yaml("Invalid YAML", "?", _))

  private[sde] def parseYaml[K <: KeyType](s: String): ZIO[Load, Error, YamlObject[K]] =
    YAML.parse(s).mapError(Error.Yaml("Invalid YAML", "?", _))

  private[sde] def parseYamlArray(bytes: Array[Byte]): ZIO[Load, Error, YamlArray] =
    YAML.parseArray(bytes).mapError(Error.Yaml("Invalid YAML", "?", _))

  private[sde] def parseYamlArray(s: String): ZIO[Load, Error, YamlArray] =
    YAML.parseArray(s).mapError(Error.Yaml("Invalid YAML", "?", _))
