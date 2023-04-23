import sbt._

object Dependencies {

  object Versions {
    val snakeyaml = "2.6"
    val tapir = "1.2.12"
    val zio = "2.0.13"
  }

  val snakeyaml = Seq(
    "org.snakeyaml" % "snakeyaml-engine" % Versions.snakeyaml,
  )

  val tapir = Seq(
    "com.softwaremill.tapir" %% "tapir-core" % Versions.tapir,
  )

  val zio = Seq(
    "dev.zio" %% "zio" % Versions.zio,
    "dev.zio" %% "zio-streams" % Versions.zio,
  )

  val `zio-test` = Seq(
    "dev.zio" %% "zio-test" % Versions.zio % Test,
  )



  val runtimeLogging = Seq(
//    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
//    "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime"
  )
}
