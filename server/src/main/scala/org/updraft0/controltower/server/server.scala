package org.updraft0.controltower.server

import org.http4s.HttpRoutes
import org.updraft0.esi.client.EsiClient
import org.updraft0.controltower.db
import org.updraft0.controltower.server.auth.SessionCrypto
import org.updraft0.controltower.server.auth.UserSession
import org.updraft0.controltower.server.db.ReferenceQueries
import org.updraft0.controltower.server.endpoints.*
import sttp.client3.UriContext
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio.{Config as _, *}

import java.nio.file.Paths

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & UserSession

  // required since http4s' `F[_]` has only a single hole
  private type EndpointIO = [A] =>> RIO[EndpointEnv, A]

  override val bootstrap = desktopLogger

  override def run =
    (ZIO.logInfo("Hello world") *> runServer)
      .provideSome(
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

  private def runServer =
    import org.http4s.blaze.server.BlazeServerBuilder
    import org.http4s.implicits.*
    import org.http4s.server.Router
    import org.http4s.server.websocket.WebSocketBuilder2
    import zio.interop.catz.*
    import zio.stream.Stream

    import scala.concurrent.ExecutionContext

    def httpRoutes: HttpRoutes[EndpointIO] = ZHttp4sServerInterpreter()
      .from(allReferenceEndpoints ++ allAuthEndpoints ++ allMapEndpoints ++ allUserEndpoints)
      .toRoutes

    for
      config   <- ZIO.service[Config]
      executor <- ZIO.executor
      _ <- BlazeServerBuilder[EndpointIO]
        .withExecutionContext(executor.asExecutionContext)
        .bindHttp(config.http.port, config.http.host)
        .withHttpApp(Router("/" -> httpRoutes).orNotFound)
        .serve
        .compile
        .drain
    yield ()
