package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import sttp.client3.SttpBackend
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.client3.logging.DefaultLog
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp.{SttpClientInterpreter, SttpClientOptions, WebSocketToPipe}
import sttp.tapir.model.UsernamePassword
import zio.{IO, Task, URIO, ZIO, ZLayer}

/** Thin HTTP client over the ESI endpoints
  */
final class EsiClient(config: EsiClient.Config, sttp: SttpClient, interp: SttpClientInterpreter):
  // request/response bodies should not be logged for JWT flow, this is why we do not use the direct client flow methods
  private val postJwtBase =
    interp
      .toSecureRequestThrowDecodeFailures(Endpoints.postJwt, Some(config.loginBase))
      .andThen(_.andThen(_.logSettings(logRequestBody = Some(false), logResponseBody = Some(false))))
      .apply(UsernamePassword(config.clientId, Some(config.clientSecret.value.asString)))

  val postJwtAuthCode: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    val reqT = postJwtBase.compose[String](code => Map("grant_type" -> "authorization_code", "code" -> code))
    i => sttp.responseMonad.map(sttp.send(reqT(i)))(_.body)

  val postJwtRefreshToken: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    val reqT = postJwtBase.compose[String](token => Map("grant_type" -> "refresh_token", "refresh_token" -> token))
    i => sttp.responseMonad.map(sttp.send(reqT(i)))(_.body)

  val getCharacterRoles: JwtString => CharacterId => Task[CharacterRoles] = jwtClient(Endpoints.getCharacterRoles)

  val getCharacterLocation: JwtString => CharacterId => Task[CharacterLocationResponse] =
    jwtClient(Endpoints.getCharacterLocation)

  val getCharacterFleet: JwtString => CharacterId => IO[FleetError, CharacterFleetResponse] =
    jwtClientDecodeErrors(Endpoints.getCharacterFleet).andThen(_.andThen(_.orDie.absolve))

  val getCharacterOnline: JwtString => CharacterId => IO[EsiError, CharacterOnlineResponse] =
    jwtClientDecodeErrors(Endpoints.getCharacterOnline).andThen(_.andThen(_.orDie.absolve))

  val getCharacterShip: JwtString => CharacterId => IO[EsiError, CharacterShipResponse] =
    jwtClientDecodeErrors(Endpoints.getCharacterShip).andThen(_.andThen(_.orDie.absolve))

  val getCharacter: CharacterId => IO[EsiError, Character] =
    interp
      .toClientThrowDecodeFailures(Endpoints.getCharacter, Some(config.base), sttp)
      .andThen(_.orDie.absolve)

  val getCharacterAffiliations: List[CharacterId] => IO[EsiError, List[CharacterAffiliation]] =
    interp
      .toClientThrowDecodeFailures(Endpoints.getCharacterAffiliations, Some(config.base), sttp)
      .andThen(_.orDie.absolve)

  private def jwtClient[A, I, E, O](e: Endpoint[A, I, E, O, Any]) =
    interp.toSecureClientThrowErrors[Task, A, I, E, O, Any](e, Some(config.base), sttp)

  private def jwtClientDecodeErrors[A, I, E, O](e: Endpoint[A, I, E, O, Any]) =
    interp.toSecureClientThrowDecodeFailures[Task, A, I, E, O, Any](e, Some(config.base), sttp)

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
