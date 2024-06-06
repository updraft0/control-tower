package org.updraft0.esi.client

import org.updraft0.controltower.constant.*

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.jsoniter.*
import sttp.tapir.model.UsernamePassword

import java.nio.charset.Charset

case class JwtString(value: String)

case class AuthErrorResponse(error: String, errorDescription: String)
case class JwtAuthResponse(accessToken: JwtString, expiresIn: Long, tokenType: String, refreshToken: String)

case class CharacterLocationResponse(solarSystemId: SystemId, stationId: Option[Int], structureId: Option[Int])
case class CharacterFleetResponse(fleetId: Long, role: String, squadId: Long, wingId: Long)

enum FleetError:
  case NotInFleet
  case Other(code: StatusCode, message: String) extends FleetError

object Endpoints:
  import jsoncodec.given
  import schema.given

  // TODO: add error codes - e.g. not found, unauthorized, token expired etc.
  private val jwtEndpoint = endpoint.securityIn(auth.bearer[String]().map(JwtString.apply)(_.value))

  // auth
  val postJwt = endpoint.post
    .in("v2" / "oauth" / "token")
    .securityIn(auth.basic[UsernamePassword]())
    .in(formBody[Map[String, String]])
    .out(jsonBody[JwtAuthResponse])
    .errorOut(jsonBody[AuthErrorResponse])

  // character
  val getCharacterRoles = jwtEndpoint.get
    .in("v3" / "characters" / path[CharacterId] / "roles")
    .out(jsonBody[CharacterRoles])
    .description("Get character corporation roles")

  val getCharacterLocation = jwtEndpoint.get
    .in("v2" / "characters" / path[CharacterId] / "location")
    .out(jsonBody[CharacterLocationResponse])
    .description("Get character location")

  val getCharacterFleet = jwtEndpoint.get
    .in("v1" / "characters" / path[CharacterId] / "fleet")
    .out(jsonBody[CharacterFleetResponse])
    .description("Get character fleet information")
    .errorOut(
      oneOf[FleetError](
        oneOfVariant(StatusCode.NotFound, emptyOutputAs(FleetError.NotInFleet).description("Pilot is not in fleet")),
        oneOfDefaultVariant(statusCode.and(stringBody(Charset.defaultCharset())).mapTo[FleetError.Other])
      )
    )

  // character no-auth

  val getCharacter = endpoint.get
    .in("v5" / "characters" / path[CharacterId])
    .out(jsonBody[Character])
    .description("Public information about a character")

  val getCharacterAffiliations = endpoint.post
    .in("v2" / "characters" / "affiliation")
    .in(jsonBody[List[CharacterId]])
    .out(jsonBody[List[CharacterAffiliation]])
    .description("Bulk lookup of character IDs to corporation, alliance and faction")
