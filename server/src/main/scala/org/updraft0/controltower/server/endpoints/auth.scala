package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.protocol.{SessionCookie as _, *}
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.controltower.server.tracking.CharacterAuthTracker
import sttp.client3.UriContext
import sttp.model.Uri
import sttp.model.headers.{Cookie, CookieValueWithMeta}
import sttp.tapir.ztapir.*
import zio.{Config as _, *}

def loginRedirect = Endpoints.loginRedirect.zServerLogic[Config & SessionCrypto]: cookieOpt =>
  for
    conf                <- ZIO.service[Config]
    sessionCookieOpt    <- cookieOpt.map(SessionCrypto.validate).getOrElse(ZIO.none)
    newSessionCookieOpt <- ZIO.when(sessionCookieOpt.isEmpty)(SessionCrypto.newSessionCookie).orDie
    callbackCode        <- SessionCrypto.callbackCode(sessionCookieOpt.orElse(newSessionCookieOpt).get)
  yield (
    oauth2LoginEndpoint(conf, callbackCode).toString,
    newSessionCookieOpt.map(toCookieValue(conf, _))
  )

def oauth2Callback = Endpoints.oauth2Callback.zServerLogic: code =>
  for
    redirectBack <- ZIO.serviceWith[Config](uiUrl)
    sessionIdOpt <- SessionCrypto.validateCallbackCode(code.state)
    _ <- sessionIdOpt match
      case None => ZIO.logWarning("no valid callback session code found, login will not proceed")
      case Some(sid) =>
        Users
          .loginCallback(code.code, sid)
          .flatMap(ca => ZIO.serviceWithZIO[CharacterAuthTracker](_.newLogin(ca)))
          .ignoreLogged
  yield redirectBack.toString

def allAuthEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    loginRedirect.widen[EndpointEnv],
    oauth2Callback.widen[EndpointEnv]
  )

private def toCookieValue(conf: Config, sessionCookie: SessionCookie): CookieValueWithMeta =
  CookieValueWithMeta.unsafeApply(
    value = sessionCookie.asProtocol.value,
    maxAge = Some(conf.auth.sessionExpiry.toSeconds),
    path = Some("/"),
    secure = true,
    httpOnly = true,
    sameSite = Some(Cookie.SameSite.Strict)
  )

private def oauth2LoginEndpoint(conf: Config, callbackCode: String): Uri =
  uri"https://${conf.auth.esi.host}/v2/oauth/authorize/"
    .withParams(
      "response_type" -> "code",
      "redirect_uri"  -> uri"${uiUrl(conf)}/api/auth/oauth2-callback".toString,
      "client_id"     -> conf.auth.esi.clientId,
      "scope"         -> conf.auth.esi.scopes.mkString(" "),
      "state"         -> callbackCode
    )

private def uiUrl(conf: Config) = uri"${conf.http.protocol}://${conf.http.host}:${conf.http.uiPort}"
