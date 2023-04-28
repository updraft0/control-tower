import Dependencies._
import build._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val `sde-reader` = project
  .in(file("sde-reader"))
  .settings(commonSettings, Seq(libraryDependencies ++= snakeyaml ++ zio ++ `zio-test`))

lazy val db = project
  .in(file("db"))
  .settings(commonSettings, Seq(
    libraryDependencies ++= flyway ++ quill ++ sqlite,
    libraryDependencies ++= zio ++ `zio-test`
  ))
  .dependsOn(`sde-reader`)

lazy val protocol =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("protocol"))
    .settings(commonSettings)

lazy val server = Project(id = "yyy-server", base = file("server"))
  .settings(
    commonSettings,
    Seq(
    )
  )
  .dependsOn(protocol.jvm, db)
  .aggregate(protocol.jvm)
