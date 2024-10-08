package org.updraft0.controltower.db

import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult
import zio.{ZIO, Config as _, *}

import java.nio.file.Files

/** Database migrations using Flyway
  */
object flyway:
  def layer: ZLayer[Config, Throwable, Vector[Flyway]] =
    ZLayer(
      ZIO.serviceWithZIO[Config](c =>
        c.dir.map(p => ZIO.attemptBlocking(Files.createDirectories(p))).getOrElse(ZIO.unit) *>
          ZIO.foreach(c.flywayConfig.databases.toVector):
            case (dbName, (dbPath, migrationFolder)) =>
              ZIO.attempt(configureFlyway(dbName, dbPath, migrationFolder, c.optParams))
      )
    )

  def migrate: ZIO[Vector[Flyway], Throwable, Vector[MigrateResult]] =
    ZIO.serviceWithZIO[Vector[Flyway]](fs => ZIO.foreach(fs)(f => migrateSingle.provide(ZLayer.succeed(f))))

  def migrateSingle: ZIO[Flyway, Throwable, MigrateResult] =
    ZIO.serviceWithZIO[Flyway](f => ZIO.attemptBlocking(f.migrate()))

private def configureFlyway(dbName: String, dbPath: String, migrationFolder: String, optParams: String) =
  Flyway
    .configure()
    .dataSource(s"${org.sqlite.JDBC.PREFIX}${optParams}", "", "") // main DS is in-memory
    .locations(migrationFolder)
    .loggers("slf4j")
    .initSql(initSql(dbName, dbPath))
    .defaultSchema(dbName)
    .schemas(dbName)
    .mixed(true)
    .load()

private def initSql(dbName: String, dbPath: String) =
  List(
    s"ATTACH DATABASE '${dbPath}' AS $dbName;",
    s"PRAGMA ${dbName}.journal_mode = WAL;"
  ).mkString(" ")
