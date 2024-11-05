package org.updraft0.controltower.db

import io.getquill.{SnakeCase, SqlInfixInterpolator}
import zio.{RIO, ZIO}

import java.sql.SQLException
import javax.sql.DataSource
import java.time.{Duration, Instant}
import java.util.concurrent.TimeoutException

package object query:
  type DbOperation[A] = ZIO[DataSource, SQLException, A]

  val ctx = SqliteJsonZioJdbcContext(SnakeCase)

  /** Run an operation within a transaction with an optional timeout
    *
    * Note: This lets you do operations in R which might not be what you want if they can take a longer time (e.g.
    * requests over network)
    */
  inline def transactionLoose[R, A](
      op: RIO[R & DataSource, A],
      timeoutOpt: Option[Duration] = None
  ): RIO[R & DataSource, A] =
    timeoutOpt match
      case None          => ZIO.logSpan("unboundedTransaction")(ctx.transaction(op))
      case Some(timeout) => ctx.transaction(op).timeoutFail(new TimeoutException("query timed out"))(timeout)

  inline def transaction[A](
      op: RIO[DataSource, A],
      timeoutOpt: Option[Duration] = None
  ): RIO[DataSource, A] = transactionLoose(op, timeoutOpt)

  inline def unixepoch                                   = sql"unixepoch() * 1000".pure.as[Instant]
  inline def unixepochMinusSeconds(inline seconds: Long) = sql"(unixepoch() - $seconds) * 1000".pure.as[Instant]
