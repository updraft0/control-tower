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

  val commonSettings = Seq(
    organization := "org.updraft0",
    version      := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.1",
    manifestSetting,
    crossVersion := CrossVersion.binary,
    scalacOptions += "-explain"
  )
}
