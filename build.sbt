val projectName = "verity"
val scala3version = "3.0.0-RC1"
val scala2version = "2.13.5"
val verityVersion = "0.1.0"

name := projectName
ThisBuild / version := verityVersion
ThisBuild / organization := "com.ysthakur"
// scalaVersion in ThisBuild := scala3version
Compile / mainClass := Some("Main")
// run / mainClass := Some("Main")

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    name := "verity",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options)
  .disablePlugins()
  .aggregate(
    `verity-ast`,
    `verity-parser`,
    `verity-codegen`,
    `verity-core`,
    `verity-common`
  ).dependsOn(`verity-ast`, `verity-parser`)

lazy val `verity-common` = project
  .in(file("verity-common"))
  .settings(
    name := "verity-common",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options
  )

lazy val `verity-ast` = project
  .in(file("verity-ast"))
  .settings(
    name := "verity-ast",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options,
    libraryDependencies ++= (Seq(
      "org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1",
      // "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3"
    ) ++ commonLibs3)
  ).dependsOn(`verity-common`)

lazy val `verity-parser` =project
  .in(file("verity-parser"))
  .settings(
    name := "verity-parser",
    scalaVersion := scala2version,
    scalacOptions ++= commonScala2Options,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "com.lihaoyi" %% "fastparse" % "2.2.2"
    ),
    scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(true)))
  ).dependsOn(`verity-ast`)

lazy val `verity-codegen` =project
  .in(file("verity-codegen"))
  .settings(
    name := "verity-codegen",
    scalaVersion := scala3version,
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3"
    )
  ).dependsOn(`verity-ast`)

lazy val `verity-core` = project
  .in(file("verity-core"))
  .settings(
    name := "verity-core",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options,
    libraryDependencies ++= (Seq(
      "org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1",
    ) ++ commonLibs3)
  ).dependsOn(`verity-ast`, `verity-codegen`, `verity-parser`)

lazy val commonLibs3 = Seq(
  //"org.scala-lang" % "scala-reflect" % scala_version,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3",
  "junit" % "junit" % "4.11" % Test,
  "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,
  "com.novocode" % "junit-interface" % "0.11" % "test",
  
)

val commonScala2Options = Seq(
  // "-language:implicitConversions"
  "-Ytasty-reader",
  "-Xsource:3"
)

val commonScala3Options = Seq(
  "-Yexplicit-nulls",
//  "-explain",
  "-Ycheck-init", //will be "-Ysafe-init"
)