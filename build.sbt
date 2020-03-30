name := "javamm-scala"

val scala_version = "0.22.0"

version in ThisBuild := "0.1.0"
organization in ThisBuild := "com.ysthakur"
scalaVersion in ThisBuild := scala_version

lazy val global = project
  .in(file("."))
  .settings(scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= commonDependencies)
  .disablePlugins()
  .aggregate(
      `javamm-parser`
  )

lazy val `javamm-parser` =
  (project in file("javamm-parser")).settings(
      name := "javamm-parser",
      libraryDependencies ++= commonDependencies,
      scalacOptions ++= commonScalacOptions
  )

lazy val commonDependencies = Seq(
    //"org.scala-lang" % "scala-reflect" % scala_version,
    "org.scalatest"  %% "scalatest"    % "3.1.1" % "test"
)

lazy val commonScalacOptions = Seq(
    "-language:implicitConversions",
    "-language:dynamics",
    "-language:reflectiveCalls",
    "-language:higherKindedTypes"
)
