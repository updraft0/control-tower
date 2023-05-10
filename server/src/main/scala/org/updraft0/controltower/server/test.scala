package org.updraft0.controltower.server

import org.updraft0.controltower.db
import org.updraft0.controltower.server.db.ReferenceQueries
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import zio.*
import org.updraft0.controltower.server.endpoints.*
import org.http4s.HttpRoutes
import javax.sql.DataSource

import java.nio.file.Paths

object TestMain extends ZIOAppDefault:
  val dbConfig = org.updraft0.controltower.db.Config(Paths.get("/tmp/scratch-ct"))

  override val bootstrap = desktopLogger

  override def run =
    (ZIO.logInfo("Hello world") *> runServer)
      .provideSome(ZLayer.succeed(dbConfig), db.postMigrationLayer)

  private type ServiceEnv = DataSource

  private def runServer = ZIO.executor.flatMap { executor =>
    import org.http4s.blaze.server.BlazeServerBuilder
    import org.http4s.server.Router
    import org.http4s.server.websocket.WebSocketBuilder2
    import org.http4s.implicits.*
    import scala.concurrent.ExecutionContext
    import zio.interop.catz._
    import zio.stream.Stream

    def httpRoutes: HttpRoutes[[A] =>> RIO[ServiceEnv, A]] = ZHttp4sServerInterpreter()
      .from(allReferenceEndpoints)
      .toRoutes

    BlazeServerBuilder[[A] =>> RIO[ServiceEnv, A]]
      .withExecutionContext(executor.asExecutionContext)
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> httpRoutes).orNotFound)
      .serve
      .compile
      .drain
  }
