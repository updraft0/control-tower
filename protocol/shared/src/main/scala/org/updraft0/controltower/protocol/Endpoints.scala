package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.{CharacterId, MapId}

import sttp.model.StatusCode
import sttp.tapir.{json as _, *}
import sttp.tapir.json.jsoniter.*

object Endpoints:
  import jsoncodec.given
  import schema.given

  // FIXME FIXME FIXME __Secure- cookie prefix - figure out why Chrome does not like it for localhost
  private[controltower] val SessionCookieName = "CT-Session"
  private val SessionCookieOpt =
    cookie[Option[String]](SessionCookieName).map(_.map(SessionCookie.apply))(_.map(_.value))
  private val SessionCookieDef = cookie[String](SessionCookieName).map(SessionCookie.apply)(_.value)

  private val RedirectHeader = header[String]("Location")

  // region Reference
  private val reference = endpoint.in("api" / "reference").errorOut(statusCode.and(stringBody))

  // opaque
  given Codec[String, CharacterId, CodecFormat.TextPlain] = Codec.long.map(CharacterId.apply)(_.asInstanceOf[Long])
  given Codec[String, MapId, CodecFormat.TextPlain]       = Codec.long.map(MapId.apply)(_.asInstanceOf[Long])

  val getSolarSystem =
    reference.get
      .in("system" / path[String]("solarSystemName"))
      .out(jsonBody[SolarSystem])
      .description("Get static data for a given solar system")

  val getSolarSystemById =
    reference.get
      .in("system" / "id" / path[Long]("solarSystemId"))
      .out(jsonBody[SolarSystem])
      .description("Get static data for a given solar system (by id)")

  val getAllSolarSystems =
    reference.get
      .in("all" / "systems")
      .out(jsonBody[ReferenceSolarSystems])
      .description("Get all solar systems")

  val getAllReference = reference.get.in("all").out(jsonBody[Reference])
  val getVersion      = reference.get.in("version").out(jsonBody[ReferenceVersion])

  val getFactions          = reference.get.in("factions").out(jsonBody[Array[Faction]])
  val getShipTypes         = reference.get.in("shipTypes").out(jsonBody[Array[ShipType]])
  val getStarTypes         = reference.get.in("starTypes").out(jsonBody[Array[StarType]])
  val getStationOperations = reference.get.in("stationOperations").out(jsonBody[Array[StationOperation]])
  val getWormholeTypes     = reference.get.in("wormholeTypes").out(jsonBody[Array[WormholeType]])
  val getSignaturesInGroup = reference.get.in("signatureGroups").out(jsonBody[Array[SignatureInGroup]])
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
    .errorOut(plainBody[String])
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

  val logoutUserCharacter = user
    .in("character" / path[CharacterId]("characterId"))
    .delete
    .description("Logs a character out from all user's sessions")

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

  val getMap =
    map.get
      .in(path[MapId]("mapId"))
      .get
      .out(jsonBody[MapInfoWithPermissions])
      .description("Get map information with permissions")

  val updateMap =
    map
      .in(path[MapId]("mapId"))
      .in(jsonBody[MapInfoWithPermissions])
      .put
      .out(jsonBody[MapInfoWithPermissions])
      .description("Update map roles")

  val deleteMap =
    map
      .in(path[MapId]("mapId"))
      .delete
      .description("Delete a map")

  // note: this is currently implemented outside of sttp using zio-http directly
  def mapWebSocket[S](using c: sttp.capabilities.Streams[S]) =
    map
      .in("ws" / path[String].name("mapName") / path[String].name("character"))
      .out(webSocketBody[MapRequest, CodecFormat.Json, MapMessage, CodecFormat.Json](c))
      .description("Open a WebSocket for map updates")

  // TODO better error for validation

  // endregion
