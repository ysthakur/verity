name := "javamm-scala"

version := "0.0.0"

val scala_version = "2.13.1"

scalaVersion := scala_version

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:dynamics")