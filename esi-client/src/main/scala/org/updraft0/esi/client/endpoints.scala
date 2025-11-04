package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import pdi.jwt.Jwt
import sttp.model.{HeaderNames, StatusCode}
import sttp.tapir.{Mapping, *}
import sttp.tapir.json.jsoniter.*
import sttp.tapir.model.UsernamePassword

import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant}

case class JwtForCharacter(characterId: CharacterId, value: String)

object JwtForCharacter:
  val Parse = Mapping.fromDecode[String, JwtForCharacter](s =>
    DecodeResult
      .fromEitherString(s, Jwt.decode(s).toEither.left.map(_.toString))
      .flatMap(c => DecodeResult.fromOption(c.subject))
      .map(cs => CharacterId.apply(cs.toLong))
      .map(charId => JwtForCharacter(charId, s))
  )(_.value)

case class AuthErrorResponse(error: String, errorDescription: String)
case class JwtAuthResponse(accessToken: JwtForCharacter, expiresIn: Long, tokenType: String, refreshToken: String)

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

enum FleetError derives CanEqual:
  case NotInFleet
  case Other(error: EsiError) extends FleetError

enum EsiError derives CanEqual:
  case BadRequest(error: String)                        extends EsiError
  case Unauthorized(error: String)                      extends EsiError
  case Forbidden(error: String, ssoStatus: Option[Int]) extends EsiError
  case RateLimited(retryAfter: Duration)                extends EsiError
  case InternalServerError(error: String)               extends EsiError
  case ServiceUnavailable(error: String)                extends EsiError
  case BadGateway()                                     extends EsiError
  case Timeout(error: String, timeout: Option[Int])     extends EsiError
  case NotFound(error: String)                          extends EsiError
  case Other(code: StatusCode, message: String)         extends EsiError

case class EsiRateLimitInfo(
    group: EsiRateLimitGroup,
    limitTotal: Int,
    limitDuration: Duration,
    limitRemaining: Int,
    limitUsed: Int
)

case class EsiRateLimitGroup(value: String) derives CanEqual

object EsiRateLimitInfo:
  def fromHeaders(group: String, limit: String, remaining: Int, used: Int): EsiRateLimitInfo =
    val limitInfo     = limit.split('/')
    val limitDuration = limitInfo(1)
    val duration      =
      if limitDuration.endsWith("m") then Duration.ofMinutes(limitDuration.slice(0, limitDuration.length - 1).toInt)
      else Duration.ofHours(limitDuration.slice(0, limitDuration.length - 1).toInt)
    EsiRateLimitInfo(
      EsiRateLimitGroup(group),
      limitTotal = limitInfo(0).toInt,
      limitDuration = duration,
      remaining,
      used
    )

  def toHeaders(info: EsiRateLimitInfo): (String, String, Int, Int) =
    val durationLimit =
      if info.limitDuration.toHoursPart > 0 then s"${info.limitDuration.toHoursPart}h"
      else s"${info.limitDuration.toSecondsPart}s"
    (info.group.value, s"${info.limitTotal}/$durationLimit", info.limitRemaining, info.limitUsed)

object Endpoints:
  import jsoncodec.given
  import schema.given

  val EsiCompatibilityDate = "2025-09-30"

  val RateLimitGroup = AttributeKey[EsiRateLimitGroup]

  private val esiErrorOut = oneOf[EsiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[EsiError.BadRequest]),
    oneOfVariant(StatusCode.Unauthorized, jsonBody[EsiError.Unauthorized]),
    oneOfVariant(StatusCode.Forbidden, jsonBody[EsiError.Forbidden]),
    oneOfVariant(
      StatusCode.TooManyRequests,
      header[Long](HeaderNames.RetryAfter).map(Duration.ofSeconds(_))(_.toSeconds).mapTo[EsiError.RateLimited]
    ),
    oneOfVariant(StatusCode.InternalServerError, jsonBody[EsiError.InternalServerError]),
    oneOfVariant(StatusCode.ServiceUnavailable, jsonBody[EsiError.ServiceUnavailable]),
    oneOfVariantClassMatcher(StatusCode.BadGateway, emptyOutputAs(EsiError.BadGateway()), EsiError.BadGateway.getClass),
    oneOfVariant(StatusCode.GatewayTimeout, jsonBody[EsiError.Timeout]),
    oneOfVariant(
      StatusCode.NotFound,
      stringBodyUtf8AnyFormat[String, CodecFormat.TextPlain](Codec.string)
        .map[EsiError.NotFound](EsiError.NotFound(_))(_.error)
    ),
    oneOfDefaultVariant(
      // TODO this still does not handle the case of getting either a string/json response back
      statusCode
        .and(stringBodyAnyFormat(Codec.string, StandardCharsets.UTF_8).description("Unknown error"))
        .mapTo[EsiError.Other]
    )
  )

  private val esiEndpointNoHeaders =
    endpoint
      .in(header("X-Compatibility-Date", EsiCompatibilityDate))
      .errorOut(esiErrorOut)

  private val rateLimitHeaders =
    header[String]("X-Ratelimit-Group")
      .and(header[String]("X-Ratelimit-Limit"))
      .and(header[Int]("X-Ratelimit-Remaining"))
      .and(header[Int]("X-Ratelimit-Used"))
      .map[EsiRateLimitInfo]((g, l, r, u) => EsiRateLimitInfo.fromHeaders(g, l, r, u))(EsiRateLimitInfo.toHeaders.apply)

  private val esiEndpoint = // public or private
    esiEndpointNoHeaders
      .out(rateLimitHeaders)

  private val jwtEndpoint = esiEndpoint
    .securityIn(auth.bearer[String]().map(JwtForCharacter.Parse))

  // auth
  val postJwt = endpoint.post
    .in("v2" / "oauth" / "token")
    .securityIn(auth.basic[UsernamePassword]())
    .in(formBody[Map[String, String]])
    .out(jsonBody[JwtAuthResponse])
    .errorOut(jsonBody[AuthErrorResponse])

  // status
  val getStatus = esiEndpointNoHeaders.get
    .in("status")
    .out(jsonBody[ServerStatusResponse])
    .description("Get server status")

  // character
  val getCharacterRoles = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "roles")
    .out(jsonBody[CharacterRoles])
    .description("Get character corporation roles")
    .attribute(RateLimitGroup, EsiRateLimitGroup("char-detail"))

  val getCharacterLocation = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "location")
    .out(jsonBody[CharacterLocationResponse])
    .description("Get character location")
    .attribute(RateLimitGroup, EsiRateLimitGroup("char-detail"))

  val getCharacterFleet = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "fleet")
    .out(jsonBody[CharacterFleetResponse])
    .description("Get character fleet information")
    .attribute(RateLimitGroup, EsiRateLimitGroup("fleet"))
    .mapErrorOut {
      case EsiError.NotFound(_) => FleetError.NotInFleet
      case x                    => FleetError.Other(x)
    } {
      case FleetError.NotInFleet => EsiError.NotFound("not in fleet")
      case FleetError.Other(x)   => x
    }

  val getCharacterOnline = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "online")
    .out(jsonBody[CharacterOnlineResponse])
    .description("Get character online status")
    .attribute(RateLimitGroup, EsiRateLimitGroup("char-location"))

  val getCharacterShip = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "ship")
    .out(jsonBody[CharacterShipResponse])
    .description("Get character's current piloted ship (if any)")
    .attribute(RateLimitGroup, EsiRateLimitGroup("char-location"))

  // search (auth)
  val search = jwtEndpoint.get
    .in("characters" / path[CharacterId] / "search")
    .in(query[List[SearchCategory]]("categories") / query[String]("search") / query[Boolean]("strict"))
    .out(jsonBody[SearchResponse])
    .description("Search for entities that match the given sub-string")
    .attribute(RateLimitGroup, EsiRateLimitGroup("char-detail"))

  // character no-auth

  val getCharacter = esiEndpointNoHeaders.get
    .in("characters" / path[CharacterId])
    .out(jsonBody[Character])
    .description("Public information about a character")

  val getCharacterAffiliations = esiEndpointNoHeaders.post
    .in("characters" / "affiliation")
    .in(jsonBody[List[CharacterId]])
    .out(jsonBody[List[CharacterAffiliation]])
    .description("Bulk lookup of character IDs to corporation, alliance and faction")

  // corporation no-auth
  val getCorporation = esiEndpointNoHeaders.get
    .in("corporations" / path[CorporationId])
    .out(jsonBody[Corporation])
    .description("Get public information about a corporation")

  // alliance no-auth
  val getAlliance = esiEndpointNoHeaders.get
    .in("alliances" / path[AllianceId])
    .out(jsonBody[Alliance])
    .description("Get public information about an alliance")

  val getAlliances = esiEndpointNoHeaders.get
    .in("alliances")
    .out(jsonBody[List[AllianceId]])
    .description("Get all active alliance ids")
