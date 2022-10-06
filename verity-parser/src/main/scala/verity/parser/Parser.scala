package verity.parser

import verity.ast.{FileNode, TextRange, Position}

import cats.parse.{Parser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer

object VerityParser {
  private val fileParser: Parser0[File => FileNode] =
    (Core.packageStmt.? ~ Core.importStmt.rep0 <* Parser.end).map {
      case (pkgStmt -> imptStmts) =>
        input => FileNode(input.getName.nn, pkgStmt, imptStmts, ???, Some(input))
    }

  def parseFile(name: String, input: File): Either[(String, Int), FileNode] = {
    fileParser.parse(
      Files.readString(input.toPath(), Charset.defaultCharset()).nn
    ) match {
      case Right((_, ast)) => Right(ast(input))
      case Left(Parser.Error(failedAtOffset, expected)) =>
        Left(
          (
            s"Syntax error at offset $failedAtOffset, expected $expected",
            failedAtOffset
          )
        )
    }
  }

  /** Just a shorter way to create TextRanges */
  private[parser] def tr(startOffset: Int, endOffset: Int) =
    TextRange(startOffset, endOffset)
}
