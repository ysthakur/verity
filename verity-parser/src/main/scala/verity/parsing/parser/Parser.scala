package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._
import TemplateDefs._

import fastparse._, JavaWhitespace._
import Parsed._
// import com.typesafe.scalalogging.Logger

import collection.mutable.ListBuffer
import java.io.{File, FileInputStream}
import java.nio.file._

object Parser {
  
  def parseFile(input: FileInputStream): Either[(String, Int), FileNode] =
    parse(input, file(_)) match {
      case Success(ast, _) => Right(ast)
      case Failure(label, index, extra) => Left((makeSyntaxErrorMsg(label, index, extra), index))
    }

  def makeSyntaxErrorMsg(label: String, index: Int, extra: Extra): String =
    s"Syntax error at offset ${extra.startIndex}, label = $label"
  
  // def parseFile(input: FileInputStream, logger: Logger): Parsed[FileNode] = {
  //   // import Parsed._
  //   val res = parse(input, file(_))
  //   res match {
  //     case f @ Failure(label, index, extra) =>
  //       logger.debug(f.toString) //todo
  //       None
  //     case _ =>
  //   }
  //   res
  // }

  // def parsePkg(name: String, subPkgs: Iterable[File], files: Iterable[File], logger: Logger): PackageNode = {
  //   val subPkgNodes = subPkgs.view.map { pkg =>
  //     val (pkgs, allFiles) = pkg.listFiles(_.isDirectory).view.partition(_.isDirectory)
  //     parsePkg(pkg.getName, pkgs, allFiles.filter(_.getName.endsWith(".verity")), logger)
  //   }

  //   val fileNodes = files.view.map(file => parseFile(new FileInputStream(file), logger)).collect {
  //     case Success(ast, _) => ast
  //   }

  //   new PackageNode(name, subPkgNodes, fileNodes)
  // }

  def file[_: P]: P[FileNode] = P(packageStmt.? ~ importStmt.rep ~ templateDef.rep ~ End).map {
    case (pkgStmt, imptStmts, templateDefs) => new FileNode(pkgStmt, imptStmts, templateDefs)
  }
}
