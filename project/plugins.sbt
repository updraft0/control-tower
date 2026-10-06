// needed due to https://github.com/sbt/sbt/issues/6997
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addDependencyTreePlugin
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.13.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.22.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("com.github.sbt"     % "sbt-native-packager"      % "1.12.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.6.2")
addSbtPlugin("org.jmotor.sbt"     % "sbt-dependency-updates"   % "1.2.9")
