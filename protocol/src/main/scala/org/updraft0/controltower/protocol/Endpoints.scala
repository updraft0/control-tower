package org.updraft0.controltower.protocol

import sttp.tapir.json.zio.*
import sttp.tapir.{json as _, *}

object Endpoints:
  import jsoncodec.given
  import schema.given

  private val reference = endpoint.in("api" / "reference").errorOut(statusCode.and(stringBody))

  val getSolarSystem =
    reference.get
      .in("system" / path[String]("solarSystemName"))
      .out(jsonBody[SolarSystem])

  val getAll     = reference.get.in("all").out(jsonBody[Reference])
  val getVersion = reference.get.in("version").out(jsonBody[Long])

  val getFactions          = reference.get.in("factions").out(jsonBody[List[Faction]])
  val getShipTypes         = reference.get.in("shipTypes").out(jsonBody[List[ShipType]])
  val getStarTypes         = reference.get.in("starTypes").out(jsonBody[List[StarType]])
  val getStationOperations = reference.get.in("stationOperations").out(jsonBody[List[StationOperation]])
  val getWormholeTypes     = reference.get.in("wormholeTypes").out(jsonBody[List[WormholeType]])
