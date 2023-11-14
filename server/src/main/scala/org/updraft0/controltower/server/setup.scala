package org.updraft0.controltower.server

import zio.*
import zio.logging.*
import zio.logging.slf4j.bridge.Slf4jBridge
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

private[server] def desktopLogFormat = {
  import LogFormat.*

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
    allAnnotations.color(LogColor.BLUE) +
    (space + label("cause", cause).highlight).filter(LogFilter.causeNonEmpty)
}

private[server] def desktopLogger = Runtime.removeDefaultLoggers >>> consoleLogger(
  ConsoleLoggerConfig(desktopLogFormat, LogFilter.logLevel(LogLevel.Debug))
) >+> Slf4jBridge.initialize
