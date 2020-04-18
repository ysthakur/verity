val projectName = "javamm-scala"
val dottyVersion = "0.23.0-RC1"
val jmmVersion = "0.1.0"

name := projectName
version in ThisBuild := jmmVersion
organization in ThisBuild := "com.ysthakur"
scalaVersion in ThisBuild := dottyVersion
mainClass in (Compile, run) := Some("com.ysthakur.Main")

lazy val root = project
    .in(file("."))
    .settings(
      name := "javamm-scala",
      scalacOptions ++= commonScalacOptions,
      libraryDependencies ++= libDeps)
    .disablePlugins()
    .aggregate(
      `javamm-parser`
    ).dependsOn(`javamm-parser`)

lazy val `javamm-parser` =
  (project in file("javamm-parser")).settings(
    name := "javamm-parser",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.13" % "1.1.2",
    scalacOptions ++= commonScalacOptions
  )

lazy val `javamm-codegen` =
  (project in file("javamm-codegen")).settings(
      name := "javamm-codegen",
    libraryDependencies ++= Seq("org.ow2.asm" % "asm" % "8.0.1", 
      "org.ow2.asm" % "asm-util" % "8.0.1")
  )

lazy val libDeps = Seq(
  //"org.scala-lang" % "scala-reflect" % scala_version,
  //"org.scalatest"  %% "scalatest"    % "3.1.1" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val commonScalacOptions = Seq(
  "-language:implicitConversions",
  "-explain"/*,
  "-Yexplicit-nulls"*/
)
