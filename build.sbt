import Dependencies._
import build._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val constant =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("constant"))
    .settings(commonSettings)

lazy val db = project
  .in(file("db"))
  .settings(commonSettings, Seq(
    libraryDependencies ++= flyway ++ quill ++ sqlite,
    libraryDependencies ++= zio ++ `zio-test`
  ))
  .dependsOn(constant.jvm, `sde-reader`)

lazy val protocol =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("protocol"))
    .settings(commonSettings, Seq(
      libraryDependencies ++= tapir,
      libraryDependencies ++= `zio-test`,
    ))
    .dependsOn(constant)

lazy val server = project
  .in(file("server"))
  .settings(commonSettings, Seq(
    libraryDependencies ++= `http4s-blaze` ++ tapir ++ `tapir-server`,
    libraryDependencies ++= zio ++ `zio-test`
  ))
  .dependsOn(protocol.jvm, db)

lazy val `sde-reader` = project
  .in(file("sde-reader"))
  .settings(commonSettings, Seq(libraryDependencies ++= snakeyaml ++ zio ++ `zio-test`))
  .dependsOn(constant.jvm)
