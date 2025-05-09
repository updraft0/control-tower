package org.updraft0.controltower.server.endpoints

import sttp.model.StatusCode
import org.updraft0.controltower.server.db
import zio.*
import java.sql.SQLException

// TODO: remove these above

private[endpoints] def dbError(error: db.Error): (StatusCode, String) =
  StatusCode.InternalServerError -> error.toString // FIXME

private[endpoints] def dbError(ex: SQLException): (StatusCode, String) =
  StatusCode.InternalServerError -> ex.toString // FIXME

private[endpoints] def logDbError(ex: SQLException)(using trace: Trace): UIO[(StatusCode, String)] =
  ZIO
    .stackTrace(using trace)
    .flatMap(st =>
      ZIO.logErrorCause("Query error", Cause.fail(ex, st)).as(StatusCode.InternalServerError -> "Query error")
    )
