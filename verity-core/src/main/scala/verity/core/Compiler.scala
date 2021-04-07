package verity.core

import scala.language.unsafeNulls

import verity.ast.*
import verity.parsing.*
import verity.parsing.parser.Parser
import verity.util.*

import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileFilter}

// def main(args: String*) = {
//   //TODO properly parse flags and stuff
//   val srcDir = args.last
// }

def compile(pkgs: Iterable[File], files: Iterable[File], options: Options) = {
  val logger = Logger("Syntax errors")
  val rootPkg = parsePkg("<root>", pkgs, files, logger)
}

def parsePkg(name: String, subPkgs: Iterable[File], files: Iterable[File], logger: Logger): PackageNode = {
  val subPkgNodes = subPkgs.view.map { pkg =>
    val (pkgs, allFiles) = pkg.listFiles((_.isDirectory): FileFilter).unsafeNN.view.partition(_.isDirectory)
    parsePkg(pkg.getName.unsafeNN, pkgs.removeNull, allFiles.removeNull.filter(_.getName.endsWith(".verity")), logger)
  }

  val fileNodes = files.view.map { file =>
    Parser.parseFile(new FileInputStream(file)) match {
      case e @ Left((errorMsg, offset)) =>
        logger.debug(errorMsg.toString) //todo
        e
      case s => s
    }
  }.collect {
    case Right(ast) => ast
  }

  new PackageNode(name, subPkgNodes, fileNodes)
}