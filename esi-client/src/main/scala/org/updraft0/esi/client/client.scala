package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.model.{HeaderNames, Uri}
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp.{SttpClientInterpreter, WebSocketToPipe}
import sttp.tapir.model.UsernamePassword
import zio.*
import zio.concurrent.ConcurrentMap

import java.time.Instant
import scala.annotation.nowarn

type SearchParams = (CharacterId, List[SearchCategory], String, Boolean)

/** Thin HTTP client over the ESI endpoints
  */
final class EsiClient(
    config: EsiClient.Config,
    sttp: SttpClient,
    interp: SttpClientInterpreter,
    retryAfter: ConcurrentMap[(CharacterId, EsiRateLimitGroup), Instant]
):

  private val UserAgent = "control-tower/0.1 https://github.com/updraft0/control-tower"

  // request/response bodies should not be logged for JWT flow, this is why we do not use the direct client flow methods
  private val postJwtBase =
    interp
      .toSecureRequestThrowDecodeFailures(Endpoints.postJwt, Some(config.loginBase))
      .andThen(
        _.andThen(
          _.header(HeaderNames.UserAgent, UserAgent)
            .logSettings(logRequestBody = Some(false), logResponseBody = Some(false))
        )
      )
      .apply(UsernamePassword(config.clientId, Some(config.clientSecret.value.asString)))

  val postJwtAuthCode: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    val reqT = postJwtBase.compose[String](code => Map("grant_type" -> "authorization_code", "code" -> code))
    i => sttp.responseMonad.map(sttp.send(reqT(i)))(_.body)

  val postJwtRefreshToken: String => Task[Either[AuthErrorResponse, JwtAuthResponse]] =
    val reqT = postJwtBase.compose[String](token => Map("grant_type" -> "refresh_token", "refresh_token" -> token))
    i => sttp.responseMonad.map(sttp.send(reqT(i)))(_.body)

  val getCharacterRoles: JwtForCharacter => CharacterId => IO[EsiError, CharacterRoles] =
    jwtRateLimit(Endpoints.getCharacterRoles)

  val getCharacterLocation: JwtForCharacter => CharacterId => IO[EsiError, CharacterLocationResponse] =
    jwtRateLimit(Endpoints.getCharacterLocation)

  val getCharacterFleet: JwtForCharacter => CharacterId => IO[FleetError, CharacterFleetResponse] =
    jwtRateLimit(Endpoints.getCharacterFleet)

  val getCharacterOnline: JwtForCharacter => CharacterId => IO[EsiError, CharacterOnlineResponse] =
    jwtRateLimit(Endpoints.getCharacterOnline)

  val getCharacterShip: JwtForCharacter => CharacterId => IO[EsiError, CharacterShipResponse] =
    jwtRateLimit(Endpoints.getCharacterShip)

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

  val search: JwtForCharacter => SearchParams => IO[EsiError, SearchResponse] =
    jwtRateLimit(Endpoints.search)

  private def noAuthClientDecodeErrors[I, O](e: Endpoint[Unit, I, EsiError, O, Any]) =
    (i: I) =>
      interp
        .toRequestThrowDecodeFailures(e, Some(config.base))
        .apply(i)
        .header(HeaderNames.UserAgent, UserAgent)
        .send(sttp)
        .orDie
        .map(_.body)
        .absolve

  // FIXME - could not reproduce this minimally and no issues on the bug tracker
  @nowarn("msg=pattern selector should be an instance of Matchable.*")
  private def jwtRateLimit[I, EsiError, O](
      e: Endpoint[JwtForCharacter, I, EsiError, (EsiRateLimitInfo, O), Any]
  ): JwtForCharacter => I => IO[EsiError, O] =
    (jwt: JwtForCharacter) =>
      (i: I) =>
        val esiMethodGroupKey =
          (jwt.characterId, e.attribute(Endpoints.RateLimitGroup).getOrElse(EsiRateLimitGroup("other")))
        for
          now           <- ZIO.clockWith(_.instant)
          retryAfterOpt <- retryAfter.get(esiMethodGroupKey)
          _             <- ZIO.when(retryAfterOpt.exists(i => now.isBefore(i)))(
            ZIO.left(EsiError.RateLimited(Duration.fromInterval(now, retryAfterOpt.get)))
          )
          req = interp
            .toSecureRequestThrowDecodeFailures[JwtForCharacter, I, EsiError, (EsiRateLimitInfo, O), Any](
              e,
              Some(config.base)
            )
            .apply(jwt)
            .apply(i)
            .header(HeaderNames.UserAgent, UserAgent)
          res <- sttp.send(req).orDie.map(_.body)
          _   <- res match
            case Left(EsiError.RateLimited(d)) => retryAfter.put(esiMethodGroupKey, now.plus(d))
            case Left(_)                       => ZIO.unit
            case Right((limitInfo, _))         =>
              retryAfter.remove(esiMethodGroupKey) *> ZIO.logDebug(limitInfo.toString) // TODO remove debug
          res2 <- ZIO.fromEither(res)
        yield res2._2

object EsiClient:
  case class Config(base: Uri, loginBase: Uri, clientId: String, clientSecret: zio.Config.Secret)

  def layer: ZLayer[Config, Throwable, EsiClient] =
    ZLayer.scoped(HttpClientZioBackend.scoped().flatMap(apply))

  def apply(sttp: SttpClient): RIO[Config, EsiClient] =
    for
      config     <- ZIO.service[Config]
      retryAfter <- ConcurrentMap.empty
    // FIXME wait until improvements land in generic aliases in layers after ZIO 2.1.1
    //      sttp   <- ZIO.service[SttpClient]
    yield new EsiClient(config, zioLoggingBackend(sttp), SttpClientInterpreter(), retryAfter)

  inline def withZIO[R, E, A](inline f: EsiClient => ZIO[R, E, A]): ZIO[R & EsiClient, E, A] =
    ZIO.serviceWithZIO[EsiClient](f.apply)
