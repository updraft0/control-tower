package org.updraft0.controltower.server

import org.updraft0.esi.client.EsiClient
import org.updraft0.controltower.db
import org.updraft0.controltower.server.auth.SessionCrypto
import org.updraft0.controltower.server.auth.UserSession
import org.updraft0.controltower.server.db.ReferenceQueries
import org.updraft0.controltower.server.endpoints.*
import sttp.client3.UriContext
import sttp.tapir.EndpointIO
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.{Config as _, *}
import zio.http.{HttpApp, Server => ZServer}

import java.nio.file.Paths

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & UserSession

  override val bootstrap = desktopLogger

  override def run =
    ZServer
      .serve[EndpointEnv](httpApp)
      .provideSome(
        httpConfigLayer,
        Config.layer,
        Config.dbConfigLayer,
        db.postMigrationLayer,
        SessionCrypto.layer,
        UserSession.layer,
        ZLayer
          .service[Config]
          .project(c =>
            EsiClient.Config(c.esi.base, uri"https://${c.auth.esi.host}", c.auth.esi.clientId, c.auth.esi.clientSecret)
          ),
        EsiClient.layer
      )

  private def httpConfigLayer: ZLayer[Config, Throwable, ZServer] =
    ZLayer(ZIO.service[Config].map(cfg => ZServer.defaultWith(_.binding(cfg.http.host, cfg.http.port)))).flatten

  private def httpApp: HttpApp[EndpointEnv] =
    ZioHttpInterpreter()
      .toHttp(allReferenceEndpoints ++ allAuthEndpoints ++ allMapEndpoints ++ allUserEndpoints)
