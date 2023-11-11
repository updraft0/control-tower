package org.updraft0.controltower.db

import io.getquill.SnakeCase
import zio.{RIO, ZIO}

import java.sql.SQLException
import javax.sql.DataSource

package object query:
  type DbOperation[A] = ZIO[DataSource, SQLException, A]

  val ctx = SqliteJsonZioJdbcContext(SnakeCase)

  // final transaction at the end
  def transaction[R, A](op: RIO[R & DataSource, A]): RIO[R & DataSource, A] = ctx.transaction(op)
