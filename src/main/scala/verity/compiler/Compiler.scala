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
      case Some(cfg) => invert(cfg.files.map(parseModule).toList)
      case None      => ???
    }
  }

  private def invert(modules: List[Ior[Errors, ModuleDef]]): Ior[Errors, List[ModuleDef]] =
    ???

  private def parseModule(file: File): Ior[Errors, ModuleDef] = {
    val filename = file.getName().nn
    if (file.isDirectory) {
      invert(file.listFiles().nn.toList.map(f => parseModule(f.nn))).map(FolderModule(filename, file, _))
    } else if (filename.endsWith(extension)) {
      Parser.parseFile(getModuleName(filename), file) match {
        case Right(mod) => Ior.right(mod)
        case Left(err) => Ior.left(Chain(err))
      }
    } else {
      Ior.left(Chain.nil)
    }
  }

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
