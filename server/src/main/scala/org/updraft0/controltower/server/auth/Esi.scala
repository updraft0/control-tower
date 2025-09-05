package org.updraft0.controltower.server.auth

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.auth.EsiError.{InvalidJwt, UpstreamAuth, UpstreamError, ValidationError}
import org.updraft0.esi.client.{AuthErrorResponse, EsiClient, JwtAuthResponse, EsiError as EsiClientError}
import pdi.jwt.{Jwt, JwtAlgorithm}
import zio.*

import java.time.Instant

enum EsiError:
  case InvalidJwt(reason: Throwable)
  case ValidationError(message: String)
  case UpstreamError(reason: Throwable)
  case UpstreamAuth(value: AuthErrorResponse)
  case ClientError(reason: EsiClientError) extends EsiError

case class EsiTokenMeta(characterId: CharacterId, characterName: String, characterOwnerHash: String, expiry: Instant)

extension (e: EsiError)
  def asThrowable: Throwable = e match
    case InvalidJwt(e)           => e
    case ValidationError(msg)    => new RuntimeException(msg)
    case UpstreamError(e)        => e
    case UpstreamAuth(err)       => new RuntimeException(s"Upstream error: ${err.error}")
    case EsiError.ClientError(e) => new RuntimeException(s"Client error: ${e}")

case class JwtEsiInfo(
    sub: String,
    tenant: String,
    azp: String,
    aud: (String, String),
    name: String,
    owner: String,
    iss: String
)

object JwtEsiInfo:
  given JsonValueCodec[JwtEsiInfo] = JsonCodecMaker.make

private val EveOnline              = "EVE Online"
private val Tranquility            = "tranquility"
private val CharacterSubjectPrefix = "CHARACTER:EVE:"

object Esi:

  /** From the authorization_code callback, extract + validate the JWT token obtained
    */
  def initialTokenAndUserData(authCode: String): ZIO[Config & EsiClient, EsiError, (JwtAuthResponse, EsiTokenMeta)] =
    EsiClient
      .withZIO(_.postJwtAuthCode(authCode))
      .mapError(EsiError.UpstreamError.apply)
      .flatMap(ZIO.fromEither(_).mapError(EsiError.UpstreamAuth.apply))
      .flatMap(extractAndValidateJwt)

  /** Update the JWT token based on refresh token (after it has expired)
    */
  def refreshTokenAndUserData(
      refreshToken: Base64
  ): ZIO[Config & EsiClient, EsiError, (JwtAuthResponse, EsiTokenMeta)] =
    EsiClient
      .withZIO(_.postJwtRefreshToken(refreshToken.stringValue))
      .mapError(EsiError.UpstreamError.apply)
      .flatMap(ZIO.fromEither(_).mapError(EsiError.UpstreamAuth.apply))
      .flatMap(extractAndValidateJwt)

private def extractAndValidateJwt(r: JwtAuthResponse): ZIO[Config, EsiError, (JwtAuthResponse, EsiTokenMeta)] =
  for
    conf  <- ZIO.service[Config]
    claim <- ZIO
      .fromTry(Jwt.decode(r.accessToken.value, conf.auth.esi.keys.rs256.key, Seq(JwtAlgorithm.RS256)))
      .mapError(EsiError.InvalidJwt.apply)
    esiInfo     <- ZIO.attempt(readFromString[JwtEsiInfo](claim.content)).mapError(EsiError.InvalidJwt.apply)
    _           <- validateEsiInfo(esiInfo, conf.auth.esi.host)
    characterId <- ZIO
      .attempt(esiInfo.sub.stripPrefix(CharacterSubjectPrefix).toLong)
      .mapError(e => EsiError.ValidationError(s"Cannot get character id: ${e}"))
    _ <- ZIO.logTrace("validated jwt")
  yield (
    r,
    EsiTokenMeta(CharacterId(characterId), esiInfo.name, esiInfo.owner, Instant.ofEpochSecond(claim.expiration.get))
  )

private def validateEsiInfo(esiInfo: JwtEsiInfo, loginHost: String): IO[EsiError, Unit] =
  ZIO.fail(EsiError.ValidationError("JWT has wrong audience")).unless(esiInfo.aud._2 == EveOnline) *>
    ZIO.fail(EsiError.ValidationError("JWT has wrong issuer")).unless(esiInfo.iss == s"https://$loginHost") *>
    ZIO.fail(EsiError.ValidationError("JWT must be issued for tranquility")).unless(esiInfo.tenant == Tranquility) *>
    ZIO
      .fail(EsiError.ValidationError("JWT must be issued for a character"))
      .unless(esiInfo.sub.startsWith(CharacterSubjectPrefix))
      .unit
