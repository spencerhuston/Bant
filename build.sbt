ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

enablePlugins(JavaAppPackaging, ScoverageSbtPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "Bant"
  )