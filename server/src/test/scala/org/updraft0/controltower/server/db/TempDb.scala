package org.updraft0.controltower.server.db

import org.updraft0.controltower.db
import zio.*
import java.nio.file.Files

object TempDb:

  private def tempDirConfigLayer: ZLayer[Any, Throwable, db.Config] =
    ZLayer.scoped(
      ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("tempdb")))(p => ZIO.unit).map(db.Config(_))
    )

  def empty: ZLayer[Any, Throwable, javax.sql.DataSource] =
    tempDirConfigLayer >>> db.postMigrationLayer
