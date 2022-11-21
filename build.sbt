val projectName = "verity"
val scala3Version = "3.2.0"
val verityVersion = "0.1.0"

name := projectName
ThisBuild / version := verityVersion
ThisBuild / organization := "com.ysthakur"
//ThisBuild / scalaVersion := scala3version
Compile / mainClass := Some("Main")
// run / mainClass := Some("Main")

Global / onChangedBuildSource := ReloadOnSourceChanges

val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
    // "-explain",
    // "-Ysafe-init",
  ),
  libraryDependencies ++= Seq(
    // "junit" % "junit" % "4.13.2" % Test,
    // "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,
    // "com.novocode" % "junit-interface" % "0.11" % Test,
    "org.scala-lang" %% "scala3-library" % scala3Version,
    "org.scalatest" %% "scalatest" % "3.2.14" % Test
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "verity",
    commonSettings
  )
  .disablePlugins()
  .aggregate(
    `verity-common`,
    `verity-ast`,
    `verity-parser`,
    `verity-core`
  )
  .dependsOn(`verity-ast`, `verity-parser`)

lazy val `verity-common` = project
  .in(file("verity-common"))
  .settings(
    name := "verity-common",
    commonSettings
  )

lazy val `verity-ast` = project
  .in(file("verity-ast"))
  .settings(
    name := "verity-ast",
    commonSettings
  ) //.dependsOn(`verity-common`)

lazy val `verity-parser` = project
  .in(file("verity-parser"))
  .settings(
    name := "verity-parser",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.8"
    )
  )
  .dependsOn(`verity-ast`)

lazy val `verity-core` = project
  .in(file("verity-core"))
  .settings(
    name := "verity-core",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "com.github.scopt" %% "scopt" % "4.1.0"
    )
  )
  .dependsOn(
    `verity-ast`,
    `verity-parser`
  )
