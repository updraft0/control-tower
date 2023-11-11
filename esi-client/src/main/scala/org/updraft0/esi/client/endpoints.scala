package org.updraft0.esi.client

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*
import sttp.tapir.model.UsernamePassword
import java.time.Instant

case class JwtString(value: String)

case class AuthErrorResponse(error: String, errorDescription: String)
case class JwtAuthResponse(accessToken: JwtString, expiresIn: Long, tokenType: String, refreshToken: String)

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

  // character no-auth

  val getCharacter = endpoint.get
    .in("v5" / "characters" / path[CharacterId])
    .out(jsonBody[Character])
    .description("Public information about a character")

  val getCharacterAffiliations = endpoint.post
    .in("v2" / "characters" / "affiliation")
    .in(jsonBody[List[Long]])
    .out(jsonBody[List[CharacterAffiliation]])
    .description("Bulk lookup of character IDs to corporation, alliance and faction")

