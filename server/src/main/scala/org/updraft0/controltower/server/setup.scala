package org.updraft0.controltower.server

import zio.{LogAnnotation as _, *}
import zio.logging.*
import zio.logging.slf4j.bridge.Slf4jBridge

import java.time.format.DateTimeFormatter
import java.util.UUID

given CanEqual[LogLevel, LogLevel] = CanEqual.derived

private[server] def desktopLogFormat = {
  import LogFormat.*

  val fileLine = make { (builder, trace, _, _, _, _, _, _, _) =>
    trace match {
      case Trace(location, file, line) =>
        builder.appendText(location)
        builder.appendText(" ")
        builder.appendText(file)
        builder.appendText(":")
        builder.appendNumeric(line)
      case _ => ()
    }
  }

  timestamp(DateTimeFormatter.ISO_LOCAL_TIME).fixed(18).color(LogColor.BLUE) |-|
    level.fixed(10).color(LogColor.YELLOW) |-|
    fiberId.fixed(15).color(LogColor.WHITE) |-|
    text(" | ") +
    line.highlight {
      case LogLevel.Error | LogLevel.Warning => LogColor.RED
      case LogLevel.Debug                    => LogColor.CYAN
      case _                                 => LogColor.WHITE
    } +
    text(" | ") +
    fileLine.color(LogColor.BLUE) +
    allAnnotations.color(LogColor.BLUE) +
    (space + label("cause", cause).highlight).filter(LogFilter.causeNonEmpty)
}

private[server] def desktopLogger = Runtime.removeDefaultLoggers >>> consoleLogger(
  ConsoleLoggerConfig(desktopLogFormat, LogFilter.LogLevelByNameConfig(LogLevel.Debug /* FIXME add config */ ))
) >+> Slf4jBridge.initialize

object Log:

  val SessionId = LogAnnotation[UUID]("sessionId", (_, r) => r, _.toString)
  val SystemId  = LogAnnotation[Long]("systemId", (_, r) => r, _.toString)
  val SystemIds = LogAnnotation[Seq[Long]]("systemIds", (l, r) => l ++ r, _.toString)
  val CharacterId =
    LogAnnotation[Long]("characterId", (_, r) => r, _.toString) // TODO https://github.com/zio/zio/issues/6829 ?
  val MapId        = LogAnnotation[Long]("mapId", (_, r) => r, _.toString)
  val MapOperation = LogAnnotation[String]("mapOperation", (_, r) => r, identity)
  val UserId       = LogAnnotation[Long]("userId", (_, r) => r, _.toString)
