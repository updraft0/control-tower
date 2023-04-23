import sbt._

object Dependencies {

  object Versions {
    val snakeyaml = "2.6"
    val sqlite    = "3.41.2.1"
    val tapir     = "1.2.12"
    val zio       = "2.0.13"
  }

  val snakeyaml = Seq(
    "org.snakeyaml" % "snakeyaml-engine" % Versions.snakeyaml
  )

  val sqlite = Seq(
    "org.xerial" % "sqlite-jdbc" % Versions.sqlite
  )

  val tapir = Seq(
    "com.softwaremill.tapir" %% "tapir-core" % Versions.tapir
  )

  val zio = Seq(
    "dev.zio" %% "zio"         % Versions.zio,
    "dev.zio" %% "zio-streams" % Versions.zio
  )

  val `zio-sql` = Seq(
    "dev.zio" %% "zio-sql" % Versions.zio
  )

  val `zio-test` = Seq(
    "dev.zio" %% "zio-test" % Versions.zio % Test
  )
}
