val projectName = "verity"
val scala_version = "3.0.0-M3"
val verityVersion = "0.1.0"

name := projectName
version in ThisBuild := verityVersion
organization in ThisBuild := "com.ysthakur"
scalaVersion in ThisBuild := scala_version
mainClass in (Compile, run) := Some("Main")

lazy val root = project
  .in(file("."))
  .settings(
    name := "verity",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= libDeps)
  .disablePlugins()
  .aggregate(
    `verity-ast`,
    `verity-parser`,
    `verity-codegen`
  ).dependsOn(`verity-ast`, `verity-parser`)

lazy val `verity-common` = project
  .in(file("verity-common"))
  .settings(
    name := "verity-common",
    scalacOptions ++= commonScalacOptions
  )

lazy val `verity-ast` = project
  .in(file("verity-ast"))
  .settings(
    name := "verity-ast",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1"
    )
  ).dependsOn(`verity-common`)

lazy val `verity-parser` =project
  .in(file("verity-parser"))
  .settings(
    name := "verity-parser",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_0.27" % "3.2.2" % Test
    )
  ).dependsOn(`verity-ast`)

lazy val `verity-codegen` =project
  .in(file("verity-codegen"))
  .settings(
    name := "verity-codegen",
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1"
    )
  )

lazy val libDeps = Seq(
  //"org.scala-lang" % "scala-reflect" % scala_version,
  //"org.scalatest"  %% "scalatest"    % "3.1.1" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val commonScalacOptions = Seq(
  "-language:implicitConversions",
//  "-explain",
  "-Yexplicit-nulls",
    "-Ycheck-init"
)
