package org.updraft0.controltower.server

import org.updraft0.controltower.db
import org.updraft0.controltower.protocol.Endpoints
import org.updraft0.controltower.server.auth.{SessionCrypto, TokenCrypto, UserSession}
import org.updraft0.controltower.server.endpoints.*
import org.updraft0.controltower.server.map.{MapConfig, MapPermissionTracker, MapReactive}
import org.updraft0.controltower.server.tracking.*
import org.updraft0.esi.client.{EsiClient, SdeClient}
import org.updraft0.minireactive.MiniReactive
import sttp.client3.UriContext
import sttp.tapir.server.interceptor.cors.*
import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import zio.http.{Response, Routes, Server as ZServer}
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.metrics.connectors.{MetricsConfig, prometheus}
import zio.metrics.jvm.DefaultJvmMetrics
import zio.{Config as _, *}

import java.security.SecureRandom

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & SdeClient & UserSession &
    MapReactive.Service & TokenCrypto & SecureRandom & MapPermissionTracker & CharacterAuthTracker & LocationTracker &
    ServerStatusTracker

  override val bootstrap = Runtime.enableRuntimeMetrics >>> desktopLogger

  override def run =
    (updateReferenceData *> ZServer
      .serve[EndpointEnv & PrometheusPublisher](httpApp ++ prometheusRouter ++ mapWebSocket))
      .provideSome(
        httpConfigLayer,
        ZLayer.succeed(MetricsConfig(5.seconds)),
        ZLayer.succeed(MapConfig()), // TODO: load from file
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
        ZLayer.service[Config].project(c => c.sde),
        SdeClient.layer,
        ZLayer
          .service[Config]
          .project(c => LocationTracker.Config(c.location.interval, c.location.parallel)),
        CharacterAuthTracker.layer,
        LocationTracker.layer,
        MapReactive.layer,
        DefaultJvmMetrics.live.unit,
        TokenCrypto.layer,
        ZLayer(ZIO.attempt(new SecureRandom())),
        MapPermissionTracker.layer,
        ServerStatusTracker.layer,
        ZLayer.scoped(CharacterAffiliationTracker.apply())
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

  private def prometheusRouter: Routes[PrometheusPublisher, Response] =
    import zio.http.*
    Routes(
      Method.GET / "metrics" -> handler(ZIO.serviceWithZIO[PrometheusPublisher](_.get.map(Response.text)))
    )

  private def httpApp: Routes[EndpointEnv, Response] =
    ZioHttpInterpreter(
      ZioHttpServerOptions.customiseInterceptors
        .corsInterceptor(CORSInterceptor.default)
        .serverLog(ZioHttpServerOptions.defaultServerLog.copy(ignoreEndpoints = Set(Endpoints.oauth2Callback)))
        .options
    )
      .toHttp(
        allReferenceEndpoints ++ allAuthEndpoints ++ allMapEndpoints ++ allUserEndpoints
      )
