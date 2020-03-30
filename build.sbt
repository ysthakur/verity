name := "javamm-scala"

version := "0.0.0"

val scala_version = "2.13.1"

scalaVersion := scala_version

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

scalacOptions ++= Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:dynamics",
    "-language:reflectiveCalls"
)