package org.updraft0.controltower.server

import org.updraft0.controltower.server.auth.{SessionCrypto, UserSession}
import org.updraft0.controltower.db.query
import org.updraft0.controltower.server.endpoints.*
import org.updraft0.controltower.server.map.MapReactive
import org.updraft0.controltower.{db, sdeloader}
import org.updraft0.esi.client.EsiClient
import org.updraft0.minireactive.MiniReactive
import sttp.client3.UriContext
import sttp.tapir.EndpointIO
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.http.{HttpApp, Server as ZServer}
import zio.{Config as _, *}

import java.nio.file.Paths

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & UserSession & MapReactive.Service

  override val bootstrap = desktopLogger

  override def run =
    (updateReferenceData *> ZServer
      .serve[EndpointEnv](httpApp))
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
        EsiClient.layer,
        MapReactive.layer
      )

  private def httpConfigLayer: ZLayer[Config, Throwable, ZServer] =
    ZLayer(ZIO.service[Config].map(cfg => ZServer.defaultWith(_.binding(cfg.http.host, cfg.http.port)))).flatten

  private def httpApp: HttpApp[EndpointEnv] =
    ZioHttpInterpreter()
      .toHttp(allReferenceEndpoints ++ allAuthEndpoints ++ allMapEndpoints ++ allUserEndpoints)

  private def updateReferenceData =
    // TODO: call the SDE loader intelligently
    ZIO.logSpan("updateReferenceData")(
      query.transaction(sdeloader.loadDerivedData) *> ZIO.logInfo("Refreshed WH static data")
    )
