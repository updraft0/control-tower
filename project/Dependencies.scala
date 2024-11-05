import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.libraryDependencies

// TODO: use %%% dependencies
//

object Dependencies {

  object Versions {
    val brotli             = "1.17.0"
    val flyway             = "9.21.1"
    val jsoniter           = "2.31.1"
    val jwt                = "10.0.1"
    val laminar            = "17.1.0"
    val laminext           = "0.17.0"
    val `native-converter` = "0.9.0"
    val quill              = "4.8.6"
    val `scala-java-time`  = "2.6.0"
    val `sjs-dom`          = "2.8.0+45-53f9a1a2-SNAPSHOT" // FIXME wait for 2.9.0 release
    val snakeyaml          = "2.8"
    val sqlite             = "3.46.1.3"
    val sttp               = "3.10.1"
    val tapir              = "1.11.8"
    val waypoint           = "8.0.1"
    val zio                = "2.1.11"
    val `zio-config`       = "4.0.2"
    val `zio-logging`      = "2.3.2"
    val `zio-metrics`      = "2.3.1"
  }

  val flyway = Seq(
    "org.flywaydb" % "flyway-core" % Versions.flyway
  )

  val jsoniter = Seq(
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % Versions.jsoniter,
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % Versions.jsoniter % "compile-internal"
  )

  val jwt = Seq(
    "com.github.jwt-scala" %% "jwt-core" % Versions.jwt
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

  val `tapir-jsoniter` = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % Versions.tapir
  )

  val `tapir-client` = Seq(
    "com.softwaremill.sttp.tapir"   %% "tapir-sttp-client" % Versions.tapir,
    "com.softwaremill.sttp.client3" %% "zio"               % Versions.sttp
  )

  val `tapir-server` = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % Versions.tapir,
    // runtime
    "com.aayushatharva.brotli4j" % "brotli4j" % Versions.brotli // TODO runtime does not play well with running from IntelliJ it seems % Runtime
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
