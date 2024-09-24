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
import zio.{Config as _, *}

import java.security.SecureRandom

/** Entrypoint into the control-tower server
  */
object Server extends ZIOAppDefault:
  type EndpointEnv = Config & javax.sql.DataSource & SessionCrypto & EsiClient & SdeClient & UserSession &
    MapReactive.Service & TokenCrypto & SecureRandom & MapPermissionTracker & CharacterAuthTracker & LocationTracker &
    ServerStatusTracker

  override val bootstrap =
    Runtime.disableFlags(RuntimeFlag.FiberRoots) >>> Runtime.enableRuntimeMetrics >>> desktopLogger

  override def run =
    (updateReferenceData *> ZServer
      .serve[EndpointEnv & PrometheusPublisher](httpApp ++ prometheusRouter ++ mapWebSocket))
      .provideSome(
        // http server + main app
        ZServer.live,
        UserSession.layer,
        MapReactive.layer,
        // configuration
        Config.layer,
        dbConfig,
        esiClientConfig,
        httpServerConfig,
        locationTrackerConfig,
        mapConfig,
        metricsConfig,
        sdeClientConfig,
        sdeConfig,
        // ESI & SDE
        EsiClient.layer,
        SdeClient.layer,
        // JWT + other crypto
        SessionCrypto.layer,
        TokenCrypto.layer,
        ZLayer(ZIO.attempt(new SecureRandom())),
        // trackers are background processes
        CharacterAffiliationTracker.layer,
        CharacterAuthTracker.layer,
        LocationTracker.layer,
        MapPermissionTracker.layer,
        ServerStatusTracker.layer,
        // database
        db.postMigrationLayer,
        // metrics
        prometheus.prometheusLayer,
        prometheus.publisherLayer
      )

  private def dbConfig: URLayer[Config, db.Config] =
    ZLayer(ZIO.serviceWith[Config](_.db))

  private def mapConfig: URLayer[Config, MapConfig] =
    ZLayer(ZIO.serviceWith[Config](_.map))

  private def metricsConfig: URLayer[Config, MetricsConfig] =
    ZLayer(ZIO.serviceWith[Config](_.metrics))

  private def locationTrackerConfig: URLayer[Config, LocationTracker.Config] =
    ZLayer(ZIO.serviceWith[Config](c => LocationTracker.Config(c.location.interval, c.location.parallel)))

  private def sdeConfig: URLayer[Config, SdeConfig] =
    ZLayer(ZIO.serviceWith[Config](_.sde))

  private def sdeClientConfig: URLayer[Config, SdeClient.Config] =
    ZLayer(ZIO.serviceWith[Config](c => SdeClient.Config(c.sde.base)))

  private def esiClientConfig: URLayer[Config, EsiClient.Config] =
    ZLayer(
      ZIO.serviceWith[Config](c =>
        EsiClient.Config(c.esi.base, uri"https://${c.auth.esi.host}", c.auth.esi.clientId, c.auth.esi.clientSecret)
      )
    )

  private def httpServerConfig: ZLayer[Config, Throwable, ZServer.Config] =
    ZLayer(ZIO.serviceWith[Config](c => c.zioHttp.binding(c.http.listenHost, c.http.port)))

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
