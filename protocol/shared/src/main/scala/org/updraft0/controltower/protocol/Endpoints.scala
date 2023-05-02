package org.updraft0.controltower.protocol

import sttp.tapir.{json as _, *}
import sttp.tapir.json.zio.*

object Server:
  import schema.given
  import json.given

  private val api = endpoint.in("api").errorOut(statusCode.and(stringBody))

  val getSolarSystem =
    api.get
      .in("solarsystem" / path[String]("solarSystemName"))
      .out(jsonBody[SolarSystem])
