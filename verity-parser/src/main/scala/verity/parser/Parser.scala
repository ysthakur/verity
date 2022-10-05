package verity.parser

import verity.ast.{FileNode, TextRange, Position}

import cats.parse.Parser

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer

private class VerityParser(inputFile: File) {
  val core = new Core
  val types = new Types(core)
  val exprs = new Exprs(core, types)
  val methods = new Methods(core, types, exprs)

  def file: Parser[FileNode] =
    (core.packageStmt.? ~ core.importStmt.rep ~ Parser.end).map {
      case (pkgStmt -> imptStmts -> templateDefs) =>
        FileNode(inputFile.getName.nn, pkgStmt, imptStmts, templateDefs, Some(inputFile))
    }
}

object VerityParser {
  def parseFile(name: String, input: File): Either[(String, Int), FileNode] = {
    val parser = new VerityParser(input)
    
    val core = new Core
    val types = new Types(core)
    val exprs = new Exprs(core, types)
    val methods = new Methods(core, types, exprs)

    val fileParser = (core.packageStmt.? ~ core.importStmt.rep0 ~ Parser.end).map {
      case (pkgStmt -> imptStmts -> templateDefs) =>
        FileNode(input.getName.nn, pkgStmt, imptStmts, templateDefs, Some(input))
    }

    fileParser.parse(Files.readString(input.toPath(), Charset.defaultCharset()).nn) match {
      case Right((_, ast)) => Right(ast)
      case Left(Parser.Error(failedAtOffset, expected)) =>
        Left((s"Syntax error at offset $failedAtOffset, expected $expected", failedAtOffset))
    }
  }

  /** Just a shorter way to create TextRanges */
  private[parser] def tr(startOffset: Int, endOffset: Int) = TextRange(startOffset, endOffset)
}
