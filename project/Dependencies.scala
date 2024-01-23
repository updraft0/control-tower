import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.libraryDependencies

// TODO: use %%% dependencies
//

object Dependencies {

  object Versions {
    val flyway             = "9.21.1"
    val jsoniter           = "2.24.4"
    val jwt                = "9.4.4"
    val laminar            = "17.0.0-M6"
    val laminext           = "0.17.0-M6"
    val `native-converter` = "0.9.0"
    val quill              = "4.8.0"
    val `scala-java-time`  = "2.5.0"
    val `sjs-dom`          = "2.8.0+3-b7b5301d-SNAPSHOT" // FIXME
    val snakeyaml          = "2.7"
    val sqlite             = "3.42.0.0"
    val sttp               = "3.9.1"
    val tapir              = "1.9.4"
    val waypoint           = "8.0.0-M2"
    val zio                = "2.0.21"
    val `zio-config`       = "4.0.1"
    val `zio-json`         = "0.6.2"
    val `zio-logging`      = "2.1.15" // TODO 2.2.0
    val `zio-metrics`      = "2.2.1"
  }

  val flyway = Seq(
    "org.flywaydb" % "flyway-core" % Versions.flyway
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
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % Versions.tapir
  )

  val zio = Seq(
    "dev.zio" %% "zio"                               % Versions.zio,
    "dev.zio" %% "zio-logging-slf4j2-bridge"         % Versions.`zio-logging`,
    "dev.zio" %% "zio-streams"                       % Versions.zio,
    "dev.zio" %% "zio-metrics-connectors"            % Versions.`zio-metrics`,
    "dev.zio" %% "zio-metrics-connectors-prometheus" % Versions.`zio-metrics`
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
