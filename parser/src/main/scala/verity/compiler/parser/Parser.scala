import verity.compiler.ast.{FileNode, TextRange, Position}

import cats.parse.{Parser => CatsParser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer

object Parser {
  private val fileParser: Parser0[File => FileNode] =
    (Core.packageStmt.? ~ Core.importStmt.rep0 <* CatsParser.end).map {
      case (pkgStmt -> imptStmts) =>
        input =>
          FileNode(input.getName.nn, pkgStmt, imptStmts, ???, Some(input))
    }

  private def processResult(
    parserResult: Either[CatsParser.Error, (String, File => FileNode)],
    inputFile: File
  ): Either[(String, Int), FileNode] =
    parserResult match {
      case Right((_, ast)) => Right(ast(inputFile))
      case Left(CatsParser.Error(failedAtOffset, expected)) =>
        Left(
          (
            s"Syntax error at offset $failedAtOffset, expected $expected",
            failedAtOffset
          )
        )
      case _ => ???
    }

  def parseString(
    name: String,
    code: String
  ): Either[(String, Int), FileNode] = {
    processResult(fileParser.parse(code), new File("asdfasdfasdfasdf"))
  }

  def parseFile(input: File): Either[(String, Int), FileNode] = {
    processResult(
      fileParser.parse(
        Files.readString(input.toPath(), Charset.defaultCharset()).nn
      ),
      input
    )
  }

  /** Just a shorter way to create TextRanges */
  private[parser] def tr(startOffset: Int, endOffset: Int) =
    TextRange(startOffset, endOffset)
}
