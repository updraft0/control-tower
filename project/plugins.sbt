// needed due to https://github.com/sbt/sbt/issues/6997
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addDependencyTreePlugin
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.14.0")
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"       % "1.0.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("com.github.sbt"     % "sbt-native-packager"      % "1.9.16")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.2")
