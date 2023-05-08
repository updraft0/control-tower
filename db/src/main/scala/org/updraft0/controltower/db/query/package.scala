package org.updraft0.controltower.db

import io.getquill.SnakeCase
import zio.{RIO, ZIO}

import java.sql.SQLException
import javax.sql.DataSource

package object query:
  type DbOperation[A] = ZIO[DataSource, SQLException, A]

  val ctx = SqliteJsonZioJdbcContext(SnakeCase)

  // final transaction at the end
  def transaction[A](op: RIO[DataSource, A]): RIO[DataSource, A] = ctx.transaction(op)
