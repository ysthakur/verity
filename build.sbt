
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.ysthakur"
ThisBuild / scalaVersion := "3.2.2"

// For Metals
Global / semanticdbEnabled := true

// Reload sbt any time build.sbt is changed
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    name := "verity",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-Xfatal-warnings",
      "-Yexplicit-nulls",
      // "-explain",
      "-Ysafe-init",
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-library" % scalaVersion.value,
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "com.github.scopt" %% "scopt" % "4.1.0",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "org.typelevel" %% "cats-core" % "2.9.0"
    )
  )
