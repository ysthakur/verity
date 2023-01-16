val projectName = "verity"
val scala3Version = "3.2.2"
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
    "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    "org.typelevel" %% "cats-core" % "2.9.0"
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
    common,
    ast,
    parser,
    core
  )
  .dependsOn(ast, parser)

lazy val common = project
  .in(file("common"))
  .settings(
    name := "verity-common",
    commonSettings
  )

lazy val ast = project
  .in(file("ast"))
  .settings(
    name := "ast",
    commonSettings
  ) //.dependsOn(`verity-common`)

lazy val parser = project
  .in(file("parser"))
  .settings(
    name := "verity-parser",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9"
    )
  )
  .dependsOn(ast)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "verity-core",
    commonSettings,
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.1.0"
    )
  )
  .dependsOn(
    ast,
    parser
  )
