import sbt._
import Keys._

object build {

  val manifestSetting = packageOptions += {
    val (title, v, vendor) = (name.value, version.value, organization.value)
    Package.ManifestAttributes(
      "Created-By"               -> "sbt",
      "Built-By"                 -> System.getProperty("user.name"),
      "Build-Jdk"                -> System.getProperty("java.version"),
      "Specification-Title"      -> title,
      "Specification-Version"    -> v,
      "Specification-Vendor"     -> vendor,
      "Implementation-Title"     -> title,
      "Implementation-Version"   -> v,
      "Implementation-Vendor-Id" -> vendor,
      "Implementation-Vendor"    -> vendor
    )
  }

  // work around intellij highlighting issues with ~ui/fastLinkJS
  val targetSettings =
    if (System.getenv("IDEA_INITIAL_DIRECTORY") ne null)
      Seq(
        target := baseDirectory.value / "target-idea"
      )
    else Seq.empty

  val commonSettings = Seq(
    organization := "org.updraft0",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.7.2",
    manifestSetting,
    crossVersion := CrossVersion.binary,
    scalacOptions ++= Seq(
      "-preview",
      "-Wunused:all",
      "-feature",
      "-deprecation",
      "-Werror",
      "-language:implicitConversions",
      "-language:strictEquality",
      "-source:future",
      "-Xmax-inlines", "64",
//      "-explain"
      "-explain-cyclic", // explain-psychic
      "-Ydebug-cyclic",
    ),
    ThisBuild / usePipelining := true,
    Test / scalacOptions --= Seq(
      "-language:strictEquality", // TODO fixup tests and remove this
      "-Wunused:all",
      "-Werror" // TODO: figure out what's causing the scala version mismatch
    ),
    Test / scalacOptions ++= Seq(
      "-Wunused:privates,locals,implicits"
    ),
  ) ++ targetSettings

}
