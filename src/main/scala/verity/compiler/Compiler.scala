package verity.compiler

import verity.compiler.ast.{FolderModule, ModuleDef, ModuleMember}
import verity.compiler.parser.Parser

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import cats.data.{Chain, Ior, NonEmptyChain, Tuple2K}
import cats.syntax.all.*
import scopt.OParser

object Compiler {

  val extension = "vt"

  /** A result and/or a bunch of results */
  type Result[T] = Ior[NonEmptyChain[CompilationError], T]

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CliConfig()) match {
      case Some(cfg) =>
        parseFiles(cfg.files) match {
          case None       => println("No source files found!")
          case Some(mods) => ???
        }
      case None => ???
    }
  }

  private def parseFiles(
    files: Seq[File]
  ): Option[Result[NonEmptyChain[ModuleDef]]] =
    val results = files.map { file =>
      val filename = file.getName().nn
      if (file.isDirectory) {
        parseFiles(file.listFiles().nn.toList.asInstanceOf[List[File]]).map(
          _.map(mods => FolderModule(filename, file, mods.toList))
        )
      } else if (filename.endsWith(extension)) {
        Some(Parser.parseFile(getModuleName(file.getName().nn), file) match {
          case Right(mod) => Ior.right(mod)
          case Left(errs) => Ior.leftNec(errs)
        })
      } else {
        None
      }
    }
    NonEmptyChain.fromSeq(results.flatten).map { results =>
      results.reduceMap(res => res.map(NonEmptyChain.one))
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
