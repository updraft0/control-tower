import Dependencies._
import build._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val constant =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("constant"))
    .settings(commonSettings)

lazy val db = project
  .in(file("db"))
  .settings(
    commonSettings,
    Seq(
      libraryDependencies ++= flyway ++ quill ++ sqlite,
      libraryDependencies ++= zio ++ `zio-test`
    )
  )
  .dependsOn(constant.jvm, `sde-reader`)

lazy val protocol =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("protocol"))
    .settings(commonSettings)
    .jvmSettings(
      libraryDependencies ++= tapir,
      libraryDependencies ++= `zio-test`
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "com.softwaremill.sttp.tapir" %%% "tapir-core"     % Versions.tapir,
        "com.softwaremill.sttp.tapir" %%% "tapir-json-zio" % Versions.tapir,
        // tests
        "dev.zio" %%% "zio-json"          % "0.5.0", // FIXME brr
        "dev.zio" %%% "zio-test"          % Versions.zio % Test,
        "dev.zio" %%% "zio-test-sbt"      % Versions.zio % Test,
        "dev.zio" %%% "zio-test-magnolia" % Versions.zio % Test
      )
    )
    .dependsOn(constant)

lazy val server = project
  .in(file("server"))
  .settings(
    commonSettings,
    Seq(
      libraryDependencies ++= `http4s-blaze` ++ tapir ++ `tapir-server`,
      libraryDependencies ++= zio ++ `zio-test`
    )
  )
  .dependsOn(protocol.jvm, db)

lazy val `sde-reader` = project
  .in(file("sde-reader"))
  .settings(commonSettings, Seq(libraryDependencies ++= snakeyaml ++ zio ++ `zio-test`))
  .dependsOn(constant.jvm)

lazy val ui = project
  .in(file("ui"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    scalaJSLinkerConfig ~= {
      _.withSourceMap(true)
    },
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      // doesn't seem to be a nice way to refer to both scala and scalajs dependencies in one place *sigh*
      "com.raquo" %%% "laminar"  % Versions.laminar,
      "com.raquo" %%% "waypoint" % Versions.waypoint,
      // test
      "dev.zio" %%% "zio-test"          % Versions.zio % Test,
      "dev.zio" %%% "zio-test-sbt"      % Versions.zio % Test,
      "dev.zio" %%% "zio-test-magnolia" % Versions.zio % Test
    )
  )
  .dependsOn(protocol.js)

lazy val root = (project in file("."))
  .aggregate(ui, server)
  .settings(
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip     := true
  )
