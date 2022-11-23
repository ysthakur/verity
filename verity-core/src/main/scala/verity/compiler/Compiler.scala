package verity.compiler

import verity.compiler.parser.Parser

import java.io.File
import scala.collection.immutable.ArraySeq

import scopt.OParser

object Compiler {

  val extension = "vt"

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CliConfig()) match {
      case Some(cfg) =>
        given proj: Project = Project()

        parseAll(cfg.files)
      case None => ???
    }
  }

  private def parseAll(files: Seq[File])(using Project): Unit = {
    for (file <- files) {
      if (file.isDirectory()) {
        parseAll(
          ArraySeq.unsafeWrapArray(file.listFiles().asInstanceOf[Array[File]])
        )
      } else if (file.getName().nn.endsWith(extension)) {
        parseFile(file)
      }
    }
  }

  private def parseFile(file: File)(using proj: Project) = {
    Parser.parseFile(file) match {
      case Right(fileNode) =>
        val pkg = fileNode.packageRef.fold(proj.root) { pkgStmt =>
          proj.getPackage(pkgStmt.path)
        }
        pkg.addFile(fileNode)
      case Left(_) => ???
    }
  }

  private val builder = OParser.builder[CliConfig]

  private val parser = {
    import builder.*

    OParser.sequence(
      programName("verity"),
      head("verity"),
      help('h', "help"),
      arg[Seq[File]]("<file|folder>...")
        .text("The files/folders to compile")
        .action((files, cfg) => cfg.copy(files = cfg.files ++ files))
    )
  }

  private case class CliConfig(
    files: Seq[File] = List.empty
  )
}
