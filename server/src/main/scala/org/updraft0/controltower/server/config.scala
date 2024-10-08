package org.updraft0.controltower.server

import org.updraft0.controltower.db.Config as DbConfig
import org.updraft0.controltower.server.map.MapConfig
import sttp.model.Uri
import zio.config.*
import zio.config.magnolia.*
import zio.http.Server as ZServer
import zio.metrics.connectors.MetricsConfig
import zio.{TaskLayer, ZIO, ZLayer}

import java.math.BigInteger
import java.security.spec.RSAPublicKeySpec
import java.security.{KeyFactory, PublicKey}
import java.time.Duration
import java.util.Base64

case class Rs256(n: String, e: String) {
  lazy val key: PublicKey = {
    val modulus  = Base64.getUrlDecoder.decode(n)
    val exponent = Base64.getUrlDecoder.decode(e)
    val spec     = new RSAPublicKeySpec(new BigInteger(1, modulus), new BigInteger(1, exponent))
    val factory  = KeyFactory.getInstance("RSA")
    factory.generatePublic(spec)
  }
}
case class EsiKeys(rs256: Rs256)
case class EsiAuthConfig(
    clientId: String,
    clientSecret: zio.Config.Secret,
    host: String,
    scopes: List[String],
    keys: EsiKeys
)
case class BytesSecret(value: zio.Config.Secret)
case class AuthConfig(
    secret: BytesSecret,
    esi: EsiAuthConfig,
    esiCallbackSecret: BytesSecret,
    sessionExpiry: Duration,
    encryptionSecret: BytesSecret
)
case class HttpConfig(protocol: String, host: String, listenHost: String, port: Int, uiPort: Int)

case class EsiConfig(base: Uri)
case class SdeConfig(base: Uri, parallel: Int)

case class LocationTrackingConfig(interval: Duration, parallel: Int)

/** Top-level ControlTower configuration (read from a HOCON file `application.conf`)
  */
case class Config(
    auth: AuthConfig,
    db: DbConfig,
    esi: EsiConfig,
    sde: SdeConfig,
    http: HttpConfig,
    location: LocationTrackingConfig,
    metrics: MetricsConfig,
    map: MapConfig,
    zioHttp: ZServer.Config
)

object Config:
  private given DeriveConfig[zio.Config.Secret]  = DeriveConfig[String].map(zio.Config.Secret.apply)
  private given DeriveConfig[java.nio.file.Path] = DeriveConfig[String].map(java.nio.file.Paths.get(_))
  private given DeriveConfig[Uri]                = DeriveConfig[String].mapAttempt(Uri.unsafeParse)
  private given DeriveConfig[BytesSecret] = DeriveConfig[zio.Config.Secret].mapOrFail:
    case s if s.value.length == 44 => Right(BytesSecret(s))
    case _                         => Left(zio.Config.Error.InvalidData(message = "Bytes secret length != 44"))

  private given DeriveConfig[DbConfig]       = DeriveConfig[java.nio.file.Path].map(DbConfig.apply)
  private given DeriveConfig[ZServer.Config] = DeriveConfig.from(ZServer.Config.config)

  def layer: TaskLayer[Config] =
    ZLayer(ZIO.configProviderWith(cp => cp.nested("control-tower").load(deriveConfig[Config].mapKey(toKebabCase))))
