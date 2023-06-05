import mill._, scalalib._

object verity extends SbtModule {
  def scalaVersion = "3.3.0"

  // Sources are in src/, not in verity/src
  def millSourcePath = build.millSourcePath

  def ivyDeps = Agg(
    ivy"org.scala-lang::scala3-library:${scalaVersion}",
    ivy"org.typelevel::cats-parse:0.3.9",
    ivy"com.github.scopt::scopt:4.1.0",
    ivy"org.typelevel::cats-core:2.9.0"
  )

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.15")
  }
}
