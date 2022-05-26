addSbtPlugin("org.scoverage"    % "sbt-scoverage"       % "1.7.2")
addSbtPlugin("org.scalameta"    % "sbt-scalafmt"        % "2.4.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.4")
libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value