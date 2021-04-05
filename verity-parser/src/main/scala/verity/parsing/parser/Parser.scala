package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._
import TemplateDefs._

import fastparse._, JavaWhitespace._
import Parsed._

import collection.mutable.ListBuffer
import java.io.{File, FileInputStream}
import java.nio.file._

object Parser {
  def parseFile(input: FileInputStream): Parsed[FileNode] = {
    // import Parsed._
    val ast = parse(input, file(_))
    ast match {
      case f@Failure(label, index, extra) => println(f)
      case _ =>
    }
    ast
  }

  //TODO figure out converting between Path and File is a good idea
  def parsePkg(pkg: File): PackageNode = {
    val subPkgs = pkg.listFiles(_.isDirectory).view.map(parsePkg)
    val files = pkg
      .listFiles(file => !file.isDirectory && file.getName.endsWith(".verity"))
      .view
      .map(file => parseFile(new FileInputStream(file)))
      .collect { case Success(ast, _) => ast }
    
    new PackageNode(subPkgs, files)
  }

  def file[_: P]: P[FileNode] = P(packageStmt.? ~ importStmt.rep ~ templateDef.rep ~ End).map {
    case (pkgStmt, imptStmts, templateDefs) => new FileNode(pkgStmt, imptStmts, templateDefs)
  }
}
