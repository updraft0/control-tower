package org.updraft0.controltower.protocol

import sttp.model.StatusCode
import sttp.tapir.json.zio.*
import sttp.tapir.{json as _, *}

object Endpoints:
  import jsoncodec.given
  import schema.given

  private[controltower] val SessionCookieName = "__Host-CT-Session"
  private val SessionCookieOpt =
    cookie[Option[String]](SessionCookieName).map(_.map(SessionCookie.apply))(_.map(_.value))
  private val SessionCookieDef = cookie[String](SessionCookieName).map(SessionCookie.apply)(_.value)

  private val RedirectHeader = header[String]("Location")

  // region Reference
  private val reference = endpoint.in("api" / "reference").errorOut(statusCode.and(stringBody))

  val getSolarSystem =
    reference.get
      .in("system" / path[String]("solarSystemName"))
      .out(jsonBody[SolarSystem])
      .description("Get static data for a given solar system")

  val getAllSolarSystems =
    reference.get
      .in("all" / "systems")
      .out(jsonBody[ReferenceSolarSystems])
      .description("Get all solar systems")

  val getAllReference = reference.get.in("all").out(jsonBody[Reference])
  val getVersion      = reference.get.in("version").out(jsonBody[Int])

  val getFactions          = reference.get.in("factions").out(jsonBody[List[Faction]])
  val getShipTypes         = reference.get.in("shipTypes").out(jsonBody[List[ShipType]])
  val getStarTypes         = reference.get.in("starTypes").out(jsonBody[List[StarType]])
  val getStationOperations = reference.get.in("stationOperations").out(jsonBody[List[StationOperation]])
  val getWormholeTypes     = reference.get.in("wormholeTypes").out(jsonBody[List[WormholeType]])
  // TODO: need an endpoint for wormhole sig strength

  // endregion

  // region Auth
  private val auth = endpoint.in("api" / "auth")

  val loginRedirect = auth.get
    .in("login")
    .in(SessionCookieOpt)
    .out(RedirectHeader)
    .out(statusCode(StatusCode.PermanentRedirect))
    .out(setCookieOpt(SessionCookieName))
    .description("Login to ESI (redirect)")

  val oauth2Callback = auth.get
    .in("oauth2-callback")
    .in(query[String]("code").and(query[String]("state")).map(CodeAndState.apply.tupled)(cs => (cs.code, cs.state)))
    .out(RedirectHeader)
    .out(statusCode(StatusCode.PermanentRedirect))
    .description("ESI OAuth2 callback (redirect)")

  private val user = endpoint
    .in("api" / "user")
    .securityIn(SessionCookieDef)
    .errorOut(plainBody[String])

  val getUserInfo = user
    .in("info")
    .out(jsonBody[UserInfo])
    .description("Return basic user information including their characters and maps")

  // endregion

  // region Map
  private val map = endpoint
    .in("api" / "map")
    .securityIn(SessionCookieDef)
    .errorOut(plainBody[String])

  val createMap =
    map.post
      .in(jsonBody[NewMap])
      .out(jsonBody[MapInfo])
      .description("Create a new map")

  // TODO: delete map

  def mapWebSocket[S](using c: sttp.capabilities.Streams[S]) =
    map
      .in(path[String].name("mapName") / path[Long].name("characterId") / "ws")
      .out(webSocketBody[MapRequest, CodecFormat.Json, MapMessage, CodecFormat.Json](c))
      .description("Open a WebSocket for map updates")

  // TODO better error for validation

  // endregion
