package org.updraft0.controltower.db

import zio.ZLayer
import javax.sql.DataSource

import java.nio.file.Path

/** Top-level database configuration
  * @param dir
  *   Directory of sqlite files
  */
case class Config(dir: Path):
  private[db] def authUrl = s"jdbc:sqlite:$dir/auth.db"
  private[db] def mapUrl = s"jdbc:sqlite:$dir/map.db"
  private[db] def sdeUrl = s"jdbc:sqlite:$dir/sde.db"

  private[db] def flywayConfig: FlywaySqliteConfig = FlywaySqliteConfig(
    Map(
      "auth" -> (s"$dir/auth.db", "auth/migration"),
      "map" -> (s"$dir/map.db", "map/migration"),
      "sde" -> (s"$dir/sde.db", "sde/migration")
    )
  )

/** Runs migrations and gives a `DataSource` that is ready to run
  */
def postMigrationLayer: ZLayer[Config, Throwable, DataSource] =
  flyway.layer >>> ZLayer.fromZIO(flyway.migrate) >>> datasource.layer

private[db] type Name            = String
private[db] type DbPath          = String
private[db] type MigrationFolder = String

private[db] case class FlywaySqliteConfig(databases: Map[Name, (DbPath, MigrationFolder)])
