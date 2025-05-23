package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.jsoniter.*
import sttp.tapir.model.UsernamePassword

import java.nio.charset.{Charset, StandardCharsets}
import java.time.Instant

case class JwtString(value: String)

case class AuthErrorResponse(error: String, errorDescription: String)
case class JwtAuthResponse(accessToken: JwtString, expiresIn: Long, tokenType: String, refreshToken: String)

case class CharacterLocationResponse(solarSystemId: SystemId, stationId: Option[Int], structureId: Option[Long])
case class CharacterFleetResponse(fleetId: Long, role: String, squadId: Long, wingId: Long)
case class CharacterOnlineResponse(
    lastLogin: Option[Instant],
    lastLogout: Option[Instant],
    logins: Option[Int],
    online: Boolean
)
case class CharacterShipResponse(
    shipItemId: Long,
    shipName: String,
    shipTypeId: Int
)
case class SearchResponse(
    agent: Option[List[Int]],
    alliance: Option[List[AllianceId]],
    character: Option[List[CharacterId]],
    constellation: Option[List[Int]],
    corporation: Option[List[CorporationId]],
    faction: Option[List[Int]],
    inventoryType: Option[List[Int]],
    region: Option[List[Int]],
    solarSystem: Option[List[Int]],
    station: Option[List[Int]],
    structure: Option[List[Long]]
)
object SearchResponse:
  val Empty: SearchResponse = SearchResponse(None, None, None, None, None, None, None, None, None, None, None)

case class ServerStatusResponse(players: Int, serverVersion: String, startTime: String, vip: Option[Boolean])

enum SearchCategory derives CanEqual:
  case Agent, Alliance, Character, Constellation, Corporation, Faction, InventoryType, Region, SolarSystem, Station,
    Structure

enum FleetError:
  case NotInFleet
  case Other(code: StatusCode, message: String) extends FleetError

enum EsiError derives CanEqual:
  case BadRequest(error: String)                        extends EsiError
  case Unauthorized(error: String)                      extends EsiError
  case Forbidden(error: String, ssoStatus: Option[Int]) extends EsiError
  case RateLimited(error: String)                       extends EsiError
  case InternalServerError(error: String)               extends EsiError
  case ServiceUnavailable(error: String)                extends EsiError
  case BadGateway()                                     extends EsiError
  case Timeout(error: String, timeout: Option[Int])     extends EsiError
  case NotFound(error: String)                          extends EsiError
  case Other(code: StatusCode, message: String)         extends EsiError

object Endpoints:
  import jsoncodec.given
  import schema.given

  // TODO: add error codes - e.g. not found, unauthorized, token expired etc.
  private val jwtEndpoint = endpoint.securityIn(auth.bearer[String]().map(JwtString.apply)(_.value))

  private val esiErrorOut = oneOf[EsiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[EsiError.BadRequest]),
    oneOfVariant(StatusCode.Unauthorized, jsonBody[EsiError.Unauthorized]),
    oneOfVariant(StatusCode.Forbidden, jsonBody[EsiError.Forbidden]),
    oneOfVariant(StatusCode(420), jsonBody[EsiError.RateLimited]),
    oneOfVariant(StatusCode.InternalServerError, jsonBody[EsiError.InternalServerError]),
    oneOfVariant(StatusCode.ServiceUnavailable, jsonBody[EsiError.ServiceUnavailable]),
    oneOfVariantClassMatcher(StatusCode.BadGateway, emptyOutputAs(EsiError.BadGateway()), EsiError.BadGateway.getClass),
    oneOfVariant(StatusCode.GatewayTimeout, jsonBody[EsiError.Timeout]),
    oneOfDefaultVariant(
      // TODO this still does not handle the case of getting either a string/json response back
      statusCode
        .and(stringBodyAnyFormat(Codec.string, StandardCharsets.UTF_8).description("Unknown error"))
        .mapTo[EsiError.Other]
    )
  )

  // auth
  val postJwt = endpoint.post
    .in("v2" / "oauth" / "token")
    .securityIn(auth.basic[UsernamePassword]())
    .in(formBody[Map[String, String]])
    .out(jsonBody[JwtAuthResponse])
    .errorOut(jsonBody[AuthErrorResponse])

  // status
  val getStatus = endpoint.get
    .in("v2" / "status")
    .out(jsonBody[ServerStatusResponse])
    .errorOut(esiErrorOut)
    .description("Get server status")

  // character
  val getCharacterRoles = jwtEndpoint.get
    .in("v3" / "characters" / path[CharacterId] / "roles")
    .out(jsonBody[CharacterRoles])
    .description("Get character corporation roles")

  val getCharacterLocation = jwtEndpoint.get
    .in("v2" / "characters" / path[CharacterId] / "location")
    .out(jsonBody[CharacterLocationResponse])
    .errorOut(esiErrorOut)
    .description("Get character location")

  val getCharacterFleet = jwtEndpoint.get
    .in("v1" / "characters" / path[CharacterId] / "fleet")
    .out(jsonBody[CharacterFleetResponse])
    .errorOut(
      oneOf[FleetError](
        oneOfVariantSingletonMatcher(
          StatusCode.NotFound,
          emptyOutputAs(FleetError.NotInFleet)
        )(FleetError.NotInFleet),
        oneOfDefaultVariant(statusCode.and(stringBody(Charset.defaultCharset())).mapTo[FleetError.Other])
      )
    )
    .description("Get character fleet information")

  val getCharacterOnline = jwtEndpoint.get
    .in("v3" / "characters" / path[CharacterId] / "online")
    .out(jsonBody[CharacterOnlineResponse])
    .errorOut(esiErrorOut)
    .description("Get character online status")

  val getCharacterShip = jwtEndpoint.get
    .in("v2" / "characters" / path[CharacterId] / "ship")
    .out(jsonBody[CharacterShipResponse])
    .errorOut(esiErrorOut)
    .description("Get character's current piloted ship (if any)")

  // search (auth)
  val search = jwtEndpoint.get
    .in("v3" / "characters" / path[CharacterId] / "search")
    .in(query[List[SearchCategory]]("categories") / query[String]("search") / query[Boolean]("strict"))
    .out(jsonBody[SearchResponse])
    .errorOut(esiErrorOut)
    .description("Search for entities that match the given sub-string")

  // character no-auth

  val getCharacter = endpoint.get
    .in("v5" / "characters" / path[CharacterId])
    .out(jsonBody[Character])
    .errorOut(oneOf[EsiError](oneOfVariant(StatusCode.NotFound, jsonBody[EsiError.NotFound]), esiErrorOut.variants*))
    .description("Public information about a character")

  val getCharacterAffiliations = endpoint.post
    .in("v2" / "characters" / "affiliation")
    .in(jsonBody[List[CharacterId]])
    .out(jsonBody[List[CharacterAffiliation]])
    .errorOut(esiErrorOut)
    .description("Bulk lookup of character IDs to corporation, alliance and faction")

  // corporation no-auth
  val getCorporation = endpoint.get
    .in("v5" / "corporations" / path[CorporationId])
    .out(jsonBody[Corporation])
    .errorOut(esiErrorOut)
    .description("Get public information about a corporation")

  // alliance no-auth
  val getAlliance = endpoint.get
    .in("v4" / "alliances" / path[AllianceId])
    .out(jsonBody[Alliance])
    .errorOut(esiErrorOut)
    .description("Get public information about an alliance")

  val getAlliances = endpoint.get
    .in("v2" / "alliances")
    .out(jsonBody[List[AllianceId]])
    .errorOut(esiErrorOut)
    .description("Get all active alliance ids")
