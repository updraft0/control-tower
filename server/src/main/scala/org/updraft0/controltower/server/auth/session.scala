package org.updraft0.controltower.server.auth

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol
import org.updraft0.controltower.server.{Config, BytesSecret}
import zio.*

import java.util.UUID
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64
import scala.util.Try

/** A session cookie is a random UUID + ':' + hmac(uuid, secret)
  */
case class SessionCookie(id: UUID, hmac: Array[Byte]):
  def asProtocol: protocol.SessionCookie =
    protocol.SessionCookie(s"${id.toString}:${Base64.getEncoder.encodeToString(hmac)}")

object SessionCookie:
  def from(proto: protocol.SessionCookie): Either[String, SessionCookie] =
    proto.value.split(':') match
      case Array(left, right) =>
        (for
          id   <- Try(UUID.fromString(left)).toEither
          hmac <- Try(Base64.getDecoder.decode(right)).toEither
        yield SessionCookie(id, hmac)).left.map(_.getMessage)
      case _ => Left("Could not split session cookie")

class SessionCrypto(private val secret: SecretKeySpec, private val callbackSecret: SecretKeySpec):
  def this(key: BytesSecret, callbackKey: BytesSecret) =
    this(secretKeySpec(key), secretKeySpec(callbackKey))

object SessionCrypto:

  def layer: ZLayer[Config, Nothing, SessionCrypto] =
    ZLayer(ZIO.serviceWith[Config](c => new SessionCrypto(c.auth.secret, c.auth.esiCallbackSecret)))

  def newSessionCookie: RIO[SessionCrypto, SessionCookie] =
    ZIO
      .attempt(UUID.randomUUID())
      .flatMap(uuid =>
        ZIO.serviceWith[SessionCrypto](sc => SessionCookie(uuid, sc.secret.hmac(uuid.toString.getBytes)))
      )

  def validate(cookie: protocol.SessionCookie): URIO[SessionCrypto, Option[SessionCookie]] =
    ZIO
      .fromEither(SessionCookie.from(cookie))
      .tapError(msg => ZIO.logWarning(s"Invalid session cookie format: ${msg}"))
      .foldZIO(
        _ => ZIO.none,
        sessionCookie =>
          ZIO.serviceWith[SessionCrypto] { sc =>
            val expected = sc.secret.hmac(sessionCookie.id.toString.getBytes)
            Option.when(expected.sameElements(sessionCookie.hmac))(sessionCookie)
          }
      )

  // note: the "callback" methods use the same encoding as the session cookie, but a different secret and the result
  //       is never encoded into a cookie (this is to prevent leaking the session cookie)
  def callbackCode(cookie: SessionCookie): URIO[SessionCrypto, String] =
    ZIO.serviceWith[SessionCrypto] { sc =>
      val newValue = SessionCookie(cookie.id, sc.callbackSecret.hmac(cookie.id.toString.getBytes))
      newValue.asProtocol.value
    }

  def validateCallbackCode(code: String): URIO[SessionCrypto, Option[UUID]] =
    ZIO
      .fromEither(SessionCookie.from(protocol.SessionCookie(code)))
      .tapError(msg => ZIO.logWarning(s"Invalid (callback) session cookie format: ${msg}"))
      .foldZIO(
        _ => ZIO.none,
        callbackCookie =>
          ZIO.serviceWith[SessionCrypto] { sc =>
            val expected = sc.callbackSecret.hmac(callbackCookie.id.toString.getBytes)
            Option.when(expected.sameElements(callbackCookie.hmac))(callbackCookie.id)
          }
      )

private val HmacSha256 = "HmacSHA256"

private def secretKeySpec(key: BytesSecret): SecretKeySpec =
  new SecretKeySpec(Base64.getDecoder.decode(key.value.value.asString), HmacSha256)

extension (s: SecretKeySpec)
  def hmac(data: Array[Byte]): Array[Byte] =
    val mac = Mac.getInstance(HmacSha256)
    mac.init(s)
    mac.doFinal(data)

case class LoggedInUser(userId: UserId, sessionId: UUID)
case class UserSession(ref: FiberRef[Option[LoggedInUser]])

object UserSession:
  def layer: ZLayer[Any, Nothing, UserSession] =
    ZLayer.scoped(FiberRef.make(Option.empty[LoggedInUser]).map(UserSession.apply))

  def setUser(user: LoggedInUser): ZIO[UserSession, Nothing, LoggedInUser] =
    ZIO.serviceWithZIO[UserSession](_.ref.set(Some(user)).as(user))

  def getUser: ZIO[UserSession, String, Option[LoggedInUser]] =
    ZIO.serviceWithZIO[UserSession](_.ref.get)
