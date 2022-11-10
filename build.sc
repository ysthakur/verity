import mill._, scalalib._, scalafmt._
import mill.contrib.bloop.Bloop

def commonScalaVersion = "3.2.0"

val commonDeps = Agg(
  ivy"org.scala-lang::scala3-library:$commonScalaVersion",
  ivy"org.typelevel::cats-core:2.8.0",
)

trait CommonModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = commonScalaVersion

  def scalacOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
    // "-Ysafe-init"
    // "-explain", // explain warnings in LOTS of detail
  )

  def ivyDeps = commonDeps

  trait CommonTests extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:3.2.14",
      ivy"org.scalatest::scalatest:3.2.14"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object `verity-ast` extends CommonModule {
  object test extends CommonTests
}

object `verity-codegen` extends CommonModule {
  object test extends CommonTests
}

object `verity-common` extends CommonModule {
  object test extends CommonTests
}

object `verity-core` extends CommonModule {
  def moduleDeps = Seq(`verity-ast`, `verity-parser`)

  object test extends CommonTests
}

object `verity-parser` extends CommonModule {
  def moduleDeps = Seq(`verity-ast`)

  def ivyDeps = commonDeps ++ Agg(ivy"org.typelevel::cats-parse:0.3.8")

  object test extends CommonTests
}

object `verity-read-bytecode` extends CommonModule {
  def scalacOptions =
    super[CommonModule].scalacOptions.filter(_ != "-Yexplicit-nulls")

  object test extends CommonTests
}
