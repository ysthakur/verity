val projectName = "verity"
val scala3version = "3.1.1"
val scala2version = "2.13.6"
val verityVersion = "0.1.0"

name := projectName
ThisBuild / version := verityVersion
ThisBuild / organization := "com.ysthakur"
//ThisBuild / scalaVersion := scala3version
Compile / mainClass := Some("Main")
// run / mainClass := Some("Main")

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(name := "verity", scalaVersion := scala3version, scalacOptions ++= commonScala3Options)
  .disablePlugins()
  .aggregate(
    `verity-common`,
    `verity-ast`,
    `verity-parser`,
    `verity-read-bytecode`,
    `verity-codegen`,
    `verity-core`
  )
  .dependsOn(`verity-ast`, `verity-parser`)

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
    libraryDependencies ++= commonLibs3
  ) //.dependsOn(`verity-common`)

lazy val `verity-parser` = project
  .in(file("verity-parser"))
  .settings(
    name := "verity-parser",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % "test",
      "org.typelevel" %% "cats-parse" % "0.3.7"
    ),
    // scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(true)))
  )
  .dependsOn(`verity-ast`)

lazy val `verity-read-bytecode` = project
  .in(file("verity-read-bytecode"))
  .settings(
    name := "verity-read-bytecode",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options.filter(_ != "-Yexplicit-nulls"),
    libraryDependencies ++= (Seq(
      "org.ow2.asm" % "asm" % "9.1",
      "org.ow2.asm" % "asm-util" % "9.1"
    ) ++ commonLibs3)
  )
  .dependsOn(`verity-common`, `verity-ast`)

lazy val `verity-codegen` = project
  .in(file("verity-codegen"))
  .settings(
    name := "verity-codegen",
    scalaVersion := scala3version,
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "9.1",
      "org.ow2.asm" % "asm-util" % "9.1"
    )
  )
  .dependsOn(`verity-ast`)

lazy val `verity-core` = project
  .in(file("verity-core"))
  .settings(
    name := "verity-core",
    scalaVersion := scala3version,
    scalacOptions ++= commonScala3Options,
    libraryDependencies ++= (Seq(
      "org.typelevel" %% "cats-core" % "2.6.1"
//      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3",
    ) ++ commonLibs3)
  )
  .dependsOn(`verity-ast`, `verity-codegen`, `verity-parser`, `verity-read-bytecode`)

lazy val commonLibs3 = Seq(
//  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3",
  "junit" % "junit" % "4.11" % Test,
  "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.scala-lang" %% "scala3-library" % scala3version
)

val commonScala2Options = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-Xlint",
  "-Xfatal-warnings",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ytasty-reader",
  "-Xsource:3"
)

val commonScala3Options = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-Xfatal-warnings",
  "-Yexplicit-nulls"
  // "-explain",
  //"-Ycheck-init", //"-Ysafe-init"
)
