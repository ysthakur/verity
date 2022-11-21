package verity.compiler

import scopt.OParser

import java.io.File

object Compiler {

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CliConfig()) match {
      case Some(cfg) =>
      case None => ???
    }
  }

  private val builder = OParser.builder[CliConfig]

  private val parser = {
    import builder.*

    OParser.sequence(
      programName("verity"),
      head("verity"),
      arg[File]("<file>...")
        .action((file, cfg) => cfg.copy(file = Some(file)))
    )
  }

  private case class CliConfig(
    file: Option[File] = None,
    printHelp: Boolean = false,
  )
}
