package verity.compiler.parser

import verity.compiler.CompilationError
import verity.compiler.ast.*
import verity.compiler.parser.Core.*

import cats.parse.{Parser => CatsParser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer

case class SyntaxError(span: Span, msg: String) extends CompilationError(span, msg)

object Parser {
  private val module: CatsParser[ModuleDef] =
    (keyword("mod") *> ws
      *> identifier
      ~ CatsParser.defer0(moduleContents: @unchecked)
      <* keyword("end").?).map { case (name, contents) =>
      SourceModule(name, contents)
    }

  private lazy val moduleContents: Parser0[List[ModuleMember]] =
    ws *> ((Core.importStmt: CatsParser[ModuleMember])
      | module | TypeDefs.typeDef | Exprs.varDef).repSep0(ws) <* ws

  private def processResult[T](
    parserResult: Either[CatsParser.Error, T]
  ): Either[SyntaxError, T] =
    (parserResult: @unchecked) match {
      case Right(ast) => Right(ast)
      case Left(CatsParser.Error(failedAtOffset, expected)) =>
        // Left(SyntaxError(s"Expected $expected", failedAtOffset))
        Left(???)
    }

  def parseModule(
    moduleName: String,
    code: String
  ): Either[SyntaxError, ModuleDef] =
    processResult(
      moduleContents
        .map { contents =>
          SourceModule(moduleName, contents)
        }
        .parseAll(code)
    )

  def parseFile(name: String, input: File): Either[SyntaxError, ModuleDef] =
    parseModule(
      name,
      Files.readString(input.toPath(), Charset.defaultCharset()).nn
    )

  /** Just a shorter way to create TextRanges */
  private[parser] def span(startOffset: Int, endOffset: Int) =
    Span(startOffset, endOffset)
}
