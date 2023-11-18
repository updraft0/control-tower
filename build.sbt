import Dependencies._
import build._
import org.scalajs.linker.interface.ModuleSplitStyle

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

lazy val `esi-client` = project
  .in(file("esi-client"))
  .settings(
    commonSettings,
    Seq(
      libraryDependencies ++= jsoniter ++ tapir ++ `tapir-client` ++ `tapir-jsoniter`,
      libraryDependencies ++= zio ++ `zio-test`
    )
  )

lazy val `mini-reactive` = project
  .in(file("mini-reactive"))
  .settings(
    commonSettings,
    Seq(
      libraryDependencies ++= zio ++ `zio-test`
    )
  )

lazy val protocol =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("protocol"))
    .settings(commonSettings)
    .jvmSettings(
      libraryDependencies ++= tapir ++ `tapir-zio-json`,
      libraryDependencies ++= `zio-test`
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "com.softwaremill.sttp.tapir" %%% "tapir-core"     % Versions.tapir,
        "com.softwaremill.sttp.tapir" %%% "tapir-json-zio" % Versions.tapir,
        // tests
        "dev.zio" %%% "zio-json"          % Versions.`zio-json`,
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
      libraryDependencies ++= jwt ++ tapir ++ `tapir-zio-json` ++ `tapir-server`,
      libraryDependencies ++= zio ++ `zio-config` ++ `zio-test`
    )
  )
  .dependsOn(protocol.jvm, db, `esi-client`, `mini-reactive`)

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
        .withModuleSplitStyle(
          // TODO: remove after dev complete?
          ModuleSplitStyle.SmallModulesFor(List("org.updraft0", "controltower"))
        )
    },
    scalaJSLinkerConfig ~= {
      _.withSourceMap(true)
    },
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      // doesn't seem to be a nice way to refer to both scala and scalajs dependencies in one place *sigh*
      "com.raquo" %%% "laminar"  % Versions.laminar,
      "com.raquo" %%% "waypoint" % Versions.waypoint,
      // laminext
      "io.laminext" %%% "core"      % Versions.laminext,
      "io.laminext" %%% "websocket" % Versions.laminext,
      // sttp
      "com.softwaremill.sttp.tapir" %%% "tapir-sttp-client" % Versions.tapir,
      "io.github.cquiroz" %%% "scala-java-time" % Versions.`scala-java-time`, // implementations of java.time classes for Scala.JS
      "com.softwaremill.sttp.client3" %%% "core" % Versions.sttp,

      // custom sjs-dom for now
      "org.scala-js" %%% "scalajs-dom" % Versions.`sjs-dom`,

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
