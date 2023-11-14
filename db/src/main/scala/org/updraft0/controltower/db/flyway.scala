package org.updraft0.controltower.db

import org.flywaydb.core.Flyway
import org.flywaydb.core.api.Location
import org.flywaydb.core.api.output.MigrateResult
import zio.{ZIO, Config as _, *}

import java.nio.file.Files

/** Database migrations using Flyway
  */
object flyway:
  def layer: ZLayer[Config, Throwable, Vector[Flyway]] =
    ZLayer(
      ZIO.serviceWithZIO[Config](c =>
        ZIO.attemptBlocking(Files.createDirectories(c.dir)) *> ZIO.foreach(c.flywayConfig.databases.toVector) {
          case (dbName, (dbPath, migrationFolder)) => ZIO.attempt(configureFlyway(dbName, dbPath, migrationFolder))
        }
      )
    )

  def migrate: ZIO[Vector[Flyway], Throwable, Vector[MigrateResult]] =
    ZIO.serviceWithZIO[Vector[Flyway]](fs => ZIO.foreach(fs)(f => migrateSingle.provide(ZLayer.succeed(f))))

  def migrateSingle: ZIO[Flyway, Throwable, MigrateResult] =
    ZIO.serviceWithZIO[Flyway](f => ZIO.attemptBlocking(f.migrate()))

private def configureFlyway(dbName: String, dbPath: String, migrationFolder: String) =
  Flyway
    .configure()
    .dataSource("jdbc:sqlite::", "", "") // main DS is in-memory
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
    s"PRAGMA ${dbName}.journal_mode = WAL;",
    s"PRAGMA foreign_keys = ON;"
  ).mkString(" ")
