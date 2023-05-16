import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.libraryDependencies

object Dependencies {

  object Versions {
    val flyway         = "9.16.3"
    val `http4s-blaze` = "0.23.14"
    val laminar        = "15.0.1"
    val quill          = "4.6.0.1"
    val snakeyaml      = "2.6"
    val sqlite         = "3.41.2.1"
    val tapir          = "1.3.0"
    val waypoint       = "6.0.0"
    val zio            = "2.0.13"
    val `zio-config`   = "4.0.0-RC15"
    val `zio-logging`  = "2.1.12"
  }

  val flyway = Seq(
    "org.flywaydb" % "flyway-core" % Versions.flyway
  )

  val `http4s-blaze` = Seq(
    "org.http4s" %% "http4s-blaze-server" % Versions.`http4s-blaze`
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
    "com.softwaremill.sttp.tapir" %% "tapir-core"     % Versions.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % Versions.tapir
  )

  val `tapir-server` = Seq(
    // zio-http has no support for websockets, yet.
    "com.softwaremill.sttp.tapir" %% "tapir-http4s-server-zio" % Versions.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-server"            % Versions.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-zio"               % Versions.tapir
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
