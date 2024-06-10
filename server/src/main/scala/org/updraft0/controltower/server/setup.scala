package org.updraft0.controltower.server

import org.updraft0.controltower.constant.*

import zio.{LogAnnotation as _, *}
import zio.config.typesafe.TypesafeConfigProvider
import zio.logging.*
import zio.logging.slf4j.bridge.Slf4jBridge

import java.util.UUID

given CanEqual[LogLevel, LogLevel] = CanEqual.derived

private[server] def configProvider = TypesafeConfigProvider.fromResourcePath()

private def consoleOrJsonLogger = ZLayer
  .fromZIO(ZIO.configProviderWith(_.nested("logger").load(Config.string("type").withDefault(""))))
  .flatMap(e => if (e.get == "json") consoleJsonLogger() else consoleLogger())

private[server] def desktopLogger = Runtime.removeDefaultLoggers >>> Runtime.setConfigProvider(
  configProvider
) >>> consoleOrJsonLogger >+> Slf4jBridge.init()

object Log:

  val SessionId = LogAnnotation[UUID]("sessionId", (_, r) => r, _.toString)
  val SystemId  = LogAnnotation[Long]("systemId", (_, r) => r, _.toString)
  val SystemIds = LogAnnotation[Seq[Long]]("systemIds", (l, r) => l ++ r, _.toString)
  val CharacterId =
    LogAnnotation[CharacterId]("characterId", (_, r) => r, _.toString) // TODO https://github.com/zio/zio/issues/6829 ?
  val MapId               = LogAnnotation[MapId]("mapId", (_, r) => r, _.toString)
  val MapOperation        = LogAnnotation[String]("mapOperation", (_, r) => r, identity)
  val BackgroundOperation = LogAnnotation[String]("bgOperation", (_, r) => r, identity)
  val UserId              = LogAnnotation[UserId]("userId", (_, r) => r, _.toString)
