val projectName = "verity"
val scala_version = "0.25.0-RC2"
val jmmVersion = "0.1.0"

name := projectName
version in ThisBuild := jmmVersion
organization in ThisBuild := "com.ysthakur"
scalaVersion in ThisBuild := scala_version
mainClass in (Compile, run) := Some("com.ysthakur.Main")

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

lazy val `verity-ast` =
  (project in file("verity-ast")).settings(
    name := "verity-ast",
    scalacOptions ++= commonScalacOptions
  )

lazy val `verity-parser` =
  (project in file("verity-parser")).settings(
    name := "verity-parser",
    scalacOptions ++= commonScalacOptions
  ).dependsOn(`verity-ast`)

lazy val `verity-codegen` =
  (project in file("verity-codegen")).settings(
      name := "verity-codegen",
    libraryDependencies ++= Seq("org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1")
  )

lazy val libDeps = Seq(
  //"org.scala-lang" % "scala-reflect" % scala_version,
  //"org.scalatest"  %% "scalatest"    % "3.1.1" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val commonScalacOptions = Seq(
  "-language:implicitConversions"/*,
  "-explain",
  "-Yexplicit-nulls"*/
)
