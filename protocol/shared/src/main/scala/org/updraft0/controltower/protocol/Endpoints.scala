package org.updraft0.controltower.protocol

import sttp.tapir.{json as _, *}
import sttp.tapir.json.zio.*

object Endpoints:
  import schema.given
  import jsoncodec.given

  private val api = endpoint.in("api").errorOut(statusCode.and(stringBody))

  val getSolarSystem =
    api.get
      .in("solarsystem" / path[String]("solarSystemName"))
      .out(jsonBody[SolarSystem])
