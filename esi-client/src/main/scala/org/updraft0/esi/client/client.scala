package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp.{SttpClientInterpreter, WebSocketToPipe}
import sttp.tapir.model.UsernamePassword
import zio.*
import java.time.Instant

type SearchParams = (CharacterId, List[SearchCategory], String, Boolean)

case class ErrorBudget(remaining: Int, resetsAt: Instant)

/** Thin HTTP client over the ESI endpoints
  */
final class EsiClient(
    config: EsiClient.Config,
    sttp: SttpClient,
    interp: SttpClientInterpreter,
    errorBudget: Ref[ErrorBudget]
):
  private val InternalRateLimit = EsiError.RateLimited("Internal rate limit due to budget")

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

  val getCharacterRoles: JwtString => CharacterId => IO[EsiError, CharacterRoles] =
    jwtCheckErrors(Endpoints.getCharacterRoles, InternalRateLimit).andThen(
      _.andThen(_.orDie.absolve.flatMap(processEsiMeta))
    )

  val getCharacterLocation: JwtString => CharacterId => IO[EsiError, CharacterLocationResponse] =
    jwtCheckErrors(Endpoints.getCharacterLocation, InternalRateLimit).andThen(
      _.andThen(_.orDie.absolve.flatMap(processEsiMeta))
    )

  val getCharacterFleet: JwtString => CharacterId => IO[FleetError, CharacterFleetResponse] =
    jwtCheckErrors(Endpoints.getCharacterFleet, FleetError.Other(InternalRateLimit))
      .andThen(_.andThen(_.orDie.absolve.flatMap(processEsiMeta)))

  val getCharacterOnline: JwtString => CharacterId => IO[EsiError, CharacterOnlineResponse] =
    jwtCheckErrors(Endpoints.getCharacterOnline, InternalRateLimit).andThen(
      _.andThen(_.orDie.absolve.flatMap(processEsiMeta))
    )

  val getCharacterShip: JwtString => CharacterId => IO[EsiError, CharacterShipResponse] =
    jwtCheckErrors(Endpoints.getCharacterShip, InternalRateLimit).andThen(
      _.andThen(_.orDie.absolve.flatMap(processEsiMeta))
    )

  val getCharacter: CharacterId => IO[EsiError, Character] =
    noAuthClientDecodeErrors(Endpoints.getCharacter)

  val getCharacterAffiliations: List[CharacterId] => IO[EsiError, List[CharacterAffiliation]] =
    noAuthClientDecodeErrors(Endpoints.getCharacterAffiliations)

  val getCorporation: CorporationId => IO[EsiError, Corporation] =
    noAuthClientDecodeErrors(Endpoints.getCorporation)

  val getAlliance: AllianceId => IO[EsiError, Alliance] =
    noAuthClientDecodeErrors(Endpoints.getAlliance)

  val getAllianceIds: Unit => IO[EsiError, List[AllianceId]] =
    noAuthClientDecodeErrors(Endpoints.getAlliances)

  val getServerStatus: Unit => IO[EsiError, ServerStatusResponse] =
    noAuthClientDecodeErrors(Endpoints.getStatus)

  val search: JwtString => SearchParams => IO[EsiError, SearchResponse] =
    jwtCheckErrors(Endpoints.search, InternalRateLimit).andThen(_.andThen(_.orDie.absolve.flatMap(processEsiMeta)))

  private def noAuthClientDecodeErrors[I, O](e: Endpoint[Unit, I, EsiError, O, Any]) =
    interp
      .toClientThrowDecodeFailures(e, Some(config.base), sttp)
      .andThen(_.orDie.absolve)

  private def jwtCheckErrors[A, I, E, O](e: Endpoint[A, I, E, O, Any], budgetError: E) =
    (a: A) =>
      (i: I) =>
        for
          now           <- ZIO.clockWith(_.instant)
          currentBudget <- errorBudget.get
          _   <- ZIO.unless(currentBudget.remaining > 0 || now.isAfter(currentBudget.resetsAt))(ZIO.left(budgetError))
          res <- interp
            .toSecureClientThrowDecodeFailures[Task, A, I, E, O, Any](e, Some(config.base), sttp)
            .apply(a)
            .apply(i)
        yield res

  private def processEsiMeta[T](value: (EsiRateLimitRemaining, EsiRateLimitSecondsLeft, T)): UIO[T] =
    for
      now <- ZIO.clockWith(_.instant)
      _   <- errorBudget.set(ErrorBudget(value._1.value, now.plusSeconds(value._2.seconds)))
    yield value._3

object EsiClient:
  case class Config(base: Uri, loginBase: Uri, clientId: String, clientSecret: zio.Config.Secret)

  def layer: ZLayer[Config, Throwable, EsiClient] =
    ZLayer.scoped(HttpClientZioBackend.scoped().flatMap(apply))

  def apply(sttp: SttpClient): RIO[Config, EsiClient] =
    for
      config <- ZIO.service[Config]
      budget <- Ref.make(ErrorBudget(999, Instant.MAX))
    // FIXME wait until improvements land in generic aliases in layers after ZIO 2.1.1
    //      sttp   <- ZIO.service[SttpClient]
    yield new EsiClient(config, zioLoggingBackend(sttp), SttpClientInterpreter(), budget)

  inline def withZIO[R, E, A](inline f: EsiClient => ZIO[R, E, A]): ZIO[R & EsiClient, E, A] =
    ZIO.serviceWithZIO[EsiClient](f.apply)
