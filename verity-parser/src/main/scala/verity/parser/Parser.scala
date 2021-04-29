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
  
  def parseFile(name: String, input: File): Either[(String, Int), FileNode] =
    parse(new FileInputStream(input), file(input)(_)) match {
      case Success(ast, _) => Right(ast)
      case Failure(label, index, extra) => Left((makeSyntaxErrorMsg(label, index, extra), index))
    }

  def makeSyntaxErrorMsg(label: String, index: Int, extra: Extra): String =
    s"Syntax error at offset ${extra.startIndex}, label = $label"

  def file[_: P](file: File): P[FileNode] = P(packageStmt.? ~ importStmt.rep ~ classlike.rep ~ End).map {
    case (pkgStmt, imptStmts, templateDefs) => new FileNode(file.getName(), pkgStmt, imptStmts, templateDefs, file)
  }
}
