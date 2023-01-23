import mill._, scalalib._

object verity extends ScalaModule {
  def scalaVersion = "3.2.2"

  def ivyDeps = Agg(
    ivy"org.scala-lang::scala3-library:${scalaVersion}",
    ivy"org.typelevel::cats-parse:0.3.9",
    ivy"com.github.scopt::scopt:4.1.0",
    ivy"org.typelevel::cats-core:2.9.0"
  )

  object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.15")
  }
}
