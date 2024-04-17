package org.updraft0.esi.client

import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.logging.{DefaultLog, LogLevel, Logger, LoggingWithResponseBodyBackend}
import zio.{Cause, Task, ZIO}

given CanEqual[LogLevel, LogLevel] = CanEqual.derived

private val zioLogger: Logger[Task] = new Logger[Task]:
  override def apply(level: LogLevel, message: => String): Task[Unit] =
    level match
      case LogLevel.Info  => ZIO.logInfo(message)
      case LogLevel.Warn  => ZIO.logWarning(message)
      case LogLevel.Debug => ZIO.logDebug(message)
      case LogLevel.Error => ZIO.logError(message)
      case LogLevel.Trace => ZIO.logTrace(message)

  override def apply(level: LogLevel, message: => String, t: Throwable): Task[Unit] =
    level match
      case LogLevel.Info  => ZIO.logInfoCause(message, Cause.fail(t))
      case LogLevel.Warn  => ZIO.logWarningCause(message, Cause.fail(t))
      case LogLevel.Debug => ZIO.logDebugCause(message, Cause.fail(t))
      case LogLevel.Error => ZIO.logErrorCause(message, Cause.fail(t))
      case LogLevel.Trace => ZIO.logTraceCause(message, Cause.fail(t))

def zioLoggingBackend(backend: SttpClient): SttpClient =
  new LoggingWithResponseBodyBackend[Task, WebSockets & ZioStreams](
    backend,
    new DefaultLog[Task](logger = zioLogger),
    includeTiming = true
  )
