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

  /** File extension for Verity code */
  val Extension = "vt"

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CliConfig()) match {
      case Some(cfg) =>
        parseFiles(cfg.files) match {
          case None       => println("No source files found!")
          case Some(result) =>
            if (Result.hasError(result)) {
              printMsgs(result.written)
            } else {
              
            }
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
        } else if (filename.endsWith(Extension)) {
          Some(Parser.parseFile(filename.stripSuffix(Extension), file))
        } else {
          None
        }
    }
    NonEmptyChain.fromSeq(results.flatten).map { results =>
      results.reduceMap(res => res.map(Chain.fromOption))
    }

  private def printMsgs(msgs: Chain[Message]): Unit = {
    for (msg <- msgs.iterator) {
      println(msg)
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
