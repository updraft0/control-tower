import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.libraryDependencies

// TODO: use %%% dependencies
//

object Dependencies {

  object Versions {
    val flyway         = "9.21.1"
    val `http4s-blaze` = "0.23.14"
    val jsoniter       = "2.23.2"
    val jwt            = "9.4.4"
    val laminar        = "16.0.0"
    val quill          = "4.8.0"
    val snakeyaml      = "2.6"
    val sqlite         = "3.42.0.0"
    val sttp           = "3.8.16"
    val tapir          = "1.6.4"
    val waypoint       = "7.0.0"
    val zio            = "2.0.18"
    val `zio-config`   = "4.0.0-RC16"
    val `zio-logging`  = "2.1.13"
  }

  val flyway = Seq(
    "org.flywaydb" % "flyway-core" % Versions.flyway
  )

  val `http4s-blaze` = Seq(
    "org.http4s" %% "http4s-blaze-server" % Versions.`http4s-blaze`
  )

  val jsoniter = Seq(
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % Versions.jsoniter,
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % Versions.jsoniter % "compile-internal"
  )

  val jwt = Seq(
    "com.github.jwt-scala" %% "jwt-zio-json" % Versions.jwt
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
    "com.softwaremill.sttp.tapir" %% "tapir-core" % Versions.tapir
  )

  val `tapir-zio-json` = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % Versions.tapir
  )

  val `tapir-jsoniter` = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % Versions.tapir
  )

  val `tapir-client` = Seq(
    "com.softwaremill.sttp.tapir"   %% "tapir-sttp-client" % Versions.tapir,
    "com.softwaremill.sttp.client3" %% "zio"               % Versions.sttp
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
    "dev.zio" %% "zio-config-magnolia" % Versions.`zio-config`,
    "dev.zio" %% "zio-config-typesafe" % Versions.`zio-config`
  )

  val `zio-test` = Seq(
    "dev.zio" %% "zio-test"          % Versions.zio % Test,
    "dev.zio" %% "zio-test-sbt"      % Versions.zio % Test,
    "dev.zio" %% "zio-test-magnolia" % Versions.zio % Test
  )
}
