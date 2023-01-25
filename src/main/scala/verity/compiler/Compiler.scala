package verity.compiler

// Necessary because of all the File stuff
import scala.language.unsafeNulls

import verity.compiler.ast.{FolderModule, ModuleDef, ModuleMember}
import verity.compiler.parser.Parser

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import cats.data.{Chain, NonEmptyChain, Writer}
import cats.syntax.all.*
import scopt.OParser

object Compiler {

  val extension = "vt"

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
  ): Option[Result[Chain[ModuleDef]]] =
    val results = files.collect {
      file =>
        val filename = file.getName()
        if (file.isDirectory) {
          parseFiles(file.listFiles().toList).map(
            _.map(mods => Some(FolderModule(filename, file, mods.toList)))
          )
        } else if (filename.endsWith(extension)) {
          Some(Parser.parseFile(getModuleName(filename), file))
        } else {
          None
        }
    }
    NonEmptyChain.fromSeq(results.flatten).map { results =>
      results.reduceMap(res => res.map(Chain.fromOption))
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
