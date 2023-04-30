import sbt._

object Dependencies {

  object Versions {
    val flyway        = "9.16.3"
    val quill         = "4.6.0.1"
    val snakeyaml     = "2.6"
    val sqlite        = "3.41.2.1"
    val tapir         = "1.2.12"
    val zio           = "2.0.13"
    val `zio-config`  = "4.0.0-RC15"
    val `zio-logging` = "2.1.12"
  }

  val flyway = Seq(
    "org.flywaydb" % "flyway-core" % Versions.flyway
  )

  val quill = Seq(
    "io.getquill" %% "quill-jdbc-zio" % Versions.quill
  )

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
    "dev.zio" %% "zio"                       % Versions.zio,
    "dev.zio" %% "zio-logging-slf4j2-bridge" % Versions.`zio-logging`,
    "dev.zio" %% "zio-streams"               % Versions.zio
  )

  val `zio-config` = Seq(
    "dev.zio" %% "zio-config"          % Versions.`zio-config`,
    "dev.zio" %% "zio-config-typesafe" % Versions.`zio-config`
  )

  val `zio-test` = Seq(
    "dev.zio" %% "zio-test"          % Versions.zio % Test,
    "dev.zio" %% "zio-test-sbt"      % Versions.zio % Test,
    "dev.zio" %% "zio-test-magnolia" % Versions.zio % Test
  )
}
