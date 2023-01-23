package verity.compiler.parser

import verity.compiler.CompilationError
import verity.compiler.ast.*
import verity.compiler.parser.Core.*

import cats.implicits.toShow
import cats.parse.{Parser => CatsParser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer
import cats.parse.LocationMap

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
  ): Either[CompilationError, T] =
    (parserResult: @unchecked) match {
      case Right(ast) => Right(ast)
      case Left(error) =>
        val expectedMsg = s"Expected one of ${error.expected.toList.map(e => s"\n- ${e.show}").mkString}"
        error.input match {
          case None => Left(CompilationError(Span.empty(Pos.atStart), expectedMsg))
          case Some(input) =>
            new LocationMap(input).toCaret(error.failedAtOffset) match {
              case None => Left(CompilationError(Span.empty(Pos.atStart), expectedMsg))
              case Some(caret) =>
                Left(CompilationError(Span(Pos(caret.offset, caret.line, caret.col), Pos(caret.offset, caret.line, caret.col + 1)), expectedMsg))
            }
        }
    }

  def parseModule(
    moduleName: String,
    code: String
  ): Either[CompilationError, ModuleDef] =
    processResult(
      moduleContents
        .map { contents =>
          SourceModule(moduleName, contents)
        }
        .parseAll(code)
    )

  def parseFile(name: String, input: File): Either[CompilationError, ModuleDef] =
    parseModule(
      name,
      Files.readString(input.toPath(), Charset.defaultCharset()).nn
    )
}
