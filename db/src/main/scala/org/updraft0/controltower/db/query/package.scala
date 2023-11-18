package org.updraft0.controltower.db

import io.getquill.{SnakeCase, SqlInfixInterpolator}
import zio.{RIO, ZIO}

import java.sql.SQLException
import javax.sql.DataSource
import java.time.Instant

package object query:
  type DbOperation[A] = ZIO[DataSource, SQLException, A]

  val ctx = SqliteJsonZioJdbcContext(SnakeCase)

  // final transaction at the end
  def transaction[R, A](op: RIO[R & DataSource, A]): RIO[R & DataSource, A] = ctx.transaction(op)

  inline def unixepoch = sql"unixepoch() * 1000".pure.as[Instant]
