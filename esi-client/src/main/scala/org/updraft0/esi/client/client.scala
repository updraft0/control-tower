package org.updraft0.esi.client

import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.model.UsernamePassword
import zio.{Task, ZIO, ZLayer}

/** Thin HTTP client over the ESI endpoints
  */
class EsiClient(config: EsiClient.Config, sttp: SttpClient, interp: SttpClientInterpreter):

  private val postJwtBase =
    interp
      .toSecureClientThrowDecodeFailures(Endpoints.postJwt, Some(config.loginBase), sttp)
      .apply(UsernamePassword(config.clientId, Some(config.clientSecret.value.asString)))

  val postJwtAuthCode: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    postJwtBase.compose[String](code => Map("grant_type" -> "authorization_code", "code" -> code))

  val postJwtRefreshToken: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    postJwtBase.compose[String](token => Map("grant_type" -> "refresh_token", "refresh_token" -> token))

  val getCharacterRoles: JwtString => CharacterId => Task[CharacterRoles] = jwtClient(Endpoints.getCharacterRoles)

  val getCharacter: CharacterId => Task[Character] =
    interp.toClientThrowErrors(Endpoints.getCharacter, Some(config.base), sttp)

  val getCharacterAffiliations: List[CharacterId] => Task[List[CharacterAffiliation]] =
    interp.toClientThrowErrors(Endpoints.getCharacterAffiliations, Some(config.base), sttp)

  private def jwtClient[A, I, E, O](e: Endpoint[A, I, E, O, Any]) =
    interp.toSecureClientThrowErrors[Task, A, I, E, O, Any](e, Some(config.base), sttp)

object EsiClient:
  case class Config(base: Uri, loginBase: Uri, clientId: String, clientSecret: zio.Config.Secret)

  def layer: ZLayer[Config, Throwable, EsiClient] =
    ZLayer.scoped(HttpClientZioBackend.scoped().flatMap(apply))

  def apply(sttp: SttpClient): ZIO[Config, Throwable, EsiClient] =
    for config <- ZIO.service[Config]
      // FIXME wait until improvements land in generic aliases in layers after ZIO 2.1.1
      //      sttp   <- ZIO.service[SttpClient]
    yield new EsiClient(config, zioLoggingBackend(sttp), SttpClientInterpreter())

  inline def withZIO[R, E, A](inline f: EsiClient => ZIO[R, E, A]): ZIO[R & EsiClient, E, A] =
    ZIO.serviceWithZIO[EsiClient](f.apply)
