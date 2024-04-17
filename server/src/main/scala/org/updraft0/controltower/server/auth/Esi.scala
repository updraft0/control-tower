package org.updraft0.controltower.server.auth

import org.updraft0.controltower.server.auth.EsiError.{InvalidJwt, UpstreamAuth, UpstreamError, ValidationError}
import org.updraft0.esi.client.{AuthErrorResponse, EsiClient, JwtAuthResponse}
import org.updraft0.controltower.server.{Config, EsiKeys}
import pdi.jwt.{JwtAlgorithm, JwtOptions, JwtZIOJson}
import pdi.jwt.algorithms.JwtAsymmetricAlgorithm
import zio.*
import zio.json.*

import java.security.PublicKey
import java.time.Instant

given CanEqual[JwtAlgorithm, JwtAlgorithm] = CanEqual.derived

enum EsiError:
  case InvalidJwt(reason: Throwable)
  case ValidationError(message: String)
  case UpstreamError(reason: Throwable)
  case UpstreamAuth(value: AuthErrorResponse)

case class EsiTokenMeta(characterId: Long, characterName: String, characterOwnerHash: String, expiry: Instant)

extension (e: EsiError)
  def asThrowable: Throwable = e match
    case InvalidJwt(e)        => e
    case ValidationError(msg) => new RuntimeException(msg)
    case UpstreamError(e)     => e
    case UpstreamAuth(err)    => new RuntimeException(s"Upstream error: ${err.error}")

case class JwtEsiInfo(
    sub: String,
    tenant: String,
    azp: String,
    aud: (String, String),
    name: String,
    owner: String,
    iss: String,
    exp: Long
)
private given JsonDecoder[JwtEsiInfo] = JsonDecoder.derived

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

private def extractAndValidateJwt(r: JwtAuthResponse): ZIO[Config, EsiError, (JwtAuthResponse, EsiTokenMeta)] =
  for
    conf    <- ZIO.service[Config]
    algoKey <- getJwtVerificationKey(conf.auth.esi.keys, r)
    js <- ZIO
      .fromTry(JwtZIOJson.decodeJson(r.accessToken.value, algoKey._2, Seq(algoKey._1)))
      .mapError(EsiError.InvalidJwt.apply)
    esiInfo <- ZIO.fromEither(js.as[JwtEsiInfo].left.map(s => EsiError.ValidationError(s"invalid json: ${s}")))
    _       <- validateEsiInfo(esiInfo, conf.auth.esi.host)
    characterId <- ZIO
      .attempt(esiInfo.sub.stripPrefix(CharacterSubjectPrefix).toLong)
      .mapError(e => EsiError.ValidationError(s"Cannot get character id: ${e}"))
    _ <- ZIO.logDebug("validated jwt") // TODO: add aspects for character id
  yield (r, EsiTokenMeta(characterId, esiInfo.name, esiInfo.owner, Instant.ofEpochSecond(esiInfo.exp)))

private def validateEsiInfo(esiInfo: JwtEsiInfo, loginHost: String): IO[EsiError, Unit] =
  ZIO.fail(EsiError.ValidationError("JWT has wrong audience")).unless(esiInfo.aud._2 == EveOnline) *>
    ZIO.fail(EsiError.ValidationError("JWT has wrong issuer")).unless(esiInfo.iss == s"https://$loginHost") *>
    ZIO.fail(EsiError.ValidationError("JWT must be issued for tranquility")).unless(esiInfo.tenant == Tranquility) *>
    ZIO
      .fail(EsiError.ValidationError("JWT must be issued for a character"))
      .unless(esiInfo.sub.startsWith(CharacterSubjectPrefix))
      .unit

// note: not sure how to get header information to extract which key we need without doing the jwt decoding twice :/
private def getJwtVerificationKey(c: EsiKeys, r: JwtAuthResponse): UIO[(JwtAsymmetricAlgorithm, PublicKey)] = {
  val rs256 = ZIO.succeed(JwtAlgorithm.RS256 -> c.rs256.key)

  ZIO
    .fromTry(
      JwtZIOJson
        .decodeRawAll(r.accessToken.value, JwtOptions(signature = false, expiration = false))
        .flatMap { (h, _, _) =>
          scala.util.Try(JwtZIOJson.parseHeader(h))
        }
    )
    .flatMap(header =>
      header.algorithm match {
        case Some(JwtAlgorithm.RS256) => rs256
        case Some(other)              => ZIO.logError(s"Unsupported algorithm ${other}, using default") *> rs256
        case None                     => ZIO.logWarning("No JWT algorithm specified, using default") *> rs256
      }
    )
    .orDie
}
