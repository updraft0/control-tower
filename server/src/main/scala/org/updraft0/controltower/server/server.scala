package org.updraft0.controltower.server

import org.updraft0.controltower.server.auth.{SessionCrypto, UserSession, TokenCrypto}
import org.updraft0.controltower.server.endpoints.*
import org.updraft0.controltower.server.map.{MapReactive, MapSessionManager}
import org.updraft0.controltower.db
import org.updraft0.esi.client.{EsiClient, SdeClient}
import org.updraft0.minireactive.MiniReactive
import sttp.client3.UriContext
import sttp.tapir.server.interceptor.cors.*
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import zio.http.{HttpApp, Server as ZServer}
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.metrics.connectors.{MetricsConfig, prometheus}
import zio.metrics.jvm.DefaultJvmMetrics
import zio.{Config as _, *}

import java.security.SecureRandom

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & SdeClient & UserSession &
    MapReactive.Service & TokenCrypto & SecureRandom & MapSessionManager

  override val bootstrap = Runtime.enableRuntimeMetrics >>> desktopLogger

  override def run =
    (updateReferenceData *> ZServer
      .serve[EndpointEnv & PrometheusPublisher](httpApp ++ prometheusRouter ++ mapWebSocket))
      .provideSome(
        httpConfigLayer,
        ZLayer.succeed(MetricsConfig(5.seconds)),
        prometheus.prometheusLayer,
        prometheus.publisherLayer,
        Config.layer,
        Config.dbConfigLayer,
        db.postMigrationLayer,
        SessionCrypto.layer,
        UserSession.layer,
        ZLayer
          .service[Config]
          .project(c =>
            EsiClient
              .Config(c.esi.base, uri"https://${c.auth.esi.host}", c.auth.esi.clientId, c.auth.esi.clientSecret)
          ),
        EsiClient.layer,
        ZLayer
          .service[Config]
          .project(c => SdeClient.Config(c.sde.base)),
        SdeClient.layer,
        MapReactive.layer,
        DefaultJvmMetrics.live.unit,
        TokenCrypto.layer,
        ZLayer(ZIO.attempt(new SecureRandom())),
        MapSessionManager.layer
      )

  private def httpConfigLayer: ZLayer[Config, Throwable, ZServer] =
    ZLayer(
      ZIO.serviceWith[Config]: cfg =>
        ZServer.defaultWith(
          _.binding(cfg.http.listenHost, cfg.http.port)
            .responseCompression(
              // TODO revisit compression options if needed
              ZServer.Config.ResponseCompressionConfig(1000, Vector(ZServer.Config.CompressionOptions.gzip(level = 1)))
            )
        )
    ).flatten

  private def prometheusRouter =
    import zio.http.*
    Routes(
      Method.GET / "metrics" -> handler(ZIO.serviceWithZIO[PrometheusPublisher](_.get.map(Response.text)))
    ).toHttpApp

  private def httpApp: HttpApp[EndpointEnv] =
    ZioHttpInterpreter(
      ZioHttpServerOptions.customiseInterceptors
        .corsInterceptor(CORSInterceptor.default)
        .options
    )
      .toHttp(
        allReferenceEndpoints ++ allAuthEndpoints ++ allMapEndpoints ++ allUserEndpoints
      )
