package org.updraft0.controltower.db

import zio.ZLayer
import javax.sql.DataSource

import java.nio.file.Path

/** Top-level database configuration
  * @param dir
  *   Directory of sqlite files (optional for in-memory configurations)
  * @param flywayConfig
  *   Configuration of flyway mappings to migration files
  * @param optParams
  *   Optional parameters to pass in the JDBC connection string
  */
class Config(val dir: Option[Path], val flywayConfig: FlywaySqliteConfig, val optParams: String = "")

object Config:
  def apply(dir: Path): Config =
    new Config(
      Some(dir),
      FlywaySqliteConfig(
        Map(
          "auth" -> (s"$dir/auth.db", "auth/migration"),
          "map"  -> (s"$dir/map.db", "map/migration"),
          "sde"  -> (s"$dir/sde.db", "sde/migration")
        )
      )
    )

/** Runs migrations and gives a `DataSource` that is ready to run
  */
def postMigrationLayer: ZLayer[Config, Throwable, DataSource] =
  flyway.layer >>> ZLayer.fromZIO(flyway.migrate) >>> datasource.layer

private[db] type Name            = String
private[db] type DbPath          = String
private[db] type MigrationFolder = String

case class FlywaySqliteConfig(databases: Map[Name, (DbPath, MigrationFolder)])
