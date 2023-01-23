package verity.compiler

import verity.compiler.ast.{FolderModule, ModuleDef, ModuleMember}
import verity.compiler.parser.Parser

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import cats.data.{Chain, Ior}
import scopt.OParser

object Compiler {

  val extension = "vt"

  type Errors = Chain[CompilationError]

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CliConfig()) match {
      case Some(cfg) => parseAll(cfg.files)
      case None      => ???
    }
  }

  private def parseModule(file: File): Ior[Errors, ModuleDef] = {
    if (file.isDirectory) {
      file.listFiles()
        .map(parseModule)
        .foldLeft(Ior.right[Errors, List[ModuleDef]](Nil)){
          (acc, mod) =>
        }
    }
    val submodules = ArrayBuffer.empty[ModuleDef]
    val syntaxErrors = ListBuffer.empty[SyntaxError]
    FolderModule(
      moduleName,
      files.collect {
        case file if (file.isDirectory()) =>
          parseAll(
            file.getName(),
            file.listFiles().asInstanceOf[Array[File]]
          )
        case file if (file.getName().nn.endsWith(extension)) =>
          Parser.parseFile(getModuleName(file.getName()), file) match {

          }
      }
    )
  }

  private def parseFile(file: File) =
    Parser.parseFile(getModuleName(file.getName()), file)

  private def getModuleName(filename: String): String =
    filename.stripSuffix(extension)

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
