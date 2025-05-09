import sbt.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.libraryDependencies

// TODO: use %%% dependencies
//

object Dependencies {

  object Versions {
    val brotli             = "1.18.0"
    val flyway             = "11.3.0" // fixed at this version for now - the jackson databind dependency is no longer optional in later versions
    val jsoniter           = "2.35.3"
    val jwt                = "10.0.4"
    val laminar            = "17.2.0"
    val `laminar-form-derivation` = "0.24.0"
    val laminext           = "0.17.0"
    val `native-converter` = "0.9.0"
    val quill              = "4.8.6"
    val `scala-java-time`  = "2.6.0"
    val `sjs-dom`          = "2.8.0+45-53f9a1a2-SNAPSHOT" // FIXME wait for 2.9.0 release
    val snakeyaml          = "2.9"
    val sqlite             = "3.49.1.0"
    val sttp               = "3.11.0"
    val tapir              = "1.11.25"
    val waypoint           = "9.0.0"
    val zio                = "2.1.17"
    val `zio-config`       = "4.0.4"
    val `zio-logging`      = "2.5.0"
    val `zio-metrics`      = "2.3.1"
    val `zio-query`        = "0.7.7"
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
    "dev.zio" %% "zio-concurrent" % Versions.zio,
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

  val `zio-query` = Seq(
    "dev.zio" %% "zio-query" % Versions.`zio-query`
  )

  val `zio-test` = Seq(
    "dev.zio" %% "zio-test"          % Versions.zio % Test,
    "dev.zio" %% "zio-test-sbt"      % Versions.zio % Test,
    "dev.zio" %% "zio-test-magnolia" % Versions.zio % Test
  )
}
