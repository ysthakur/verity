package verity.compiler.parser

import verity.compiler.{Message, Result}
import verity.compiler.ast.*
import verity.compiler.parser.Core.*

import cats.implicits.toShow
import cats.parse.{Parser => CatsParser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer
import cats.parse.LocationMap
import cats.data.{Chain, Writer}

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
    filename: String,
    parserResult: Either[CatsParser.Error, T]
  ): Result[Option[T]] =
    (parserResult: @unchecked) match {
      case Right(ast) => Writer.value(Some(ast))
      case Left(error) =>
        val span = error.input match {
          case None =>
            Span.empty(Pos.atStart)
          case Some(input) =>
            new LocationMap(input).toCaret(error.failedAtOffset) match {
              case None =>
                Span.empty(Pos.atStart)
              case Some(caret) =>
                Span(
                  Pos(caret.offset, caret.line, caret.col),
                  Pos(caret.offset, caret.line, caret.col + 1)
                )
            }
        }
        Result.msg(
          Message.err(
            s"Expected one of ${error.expected.toList.map(e => s"\n- ${e.show}").mkString}",
            span,
            filename
          )
        )
    }

  def parseModule(
    filename: String,
    moduleName: String,
    code: String
  ): Result[Option[ModuleDef]] =
    processResult(
      filename,
      moduleContents
        .map { contents =>
          SourceModule(moduleName, contents)
        }
        .parseAll(code)
    )

  def parseFile(name: String, input: File): Result[Option[ModuleDef]] =
    parseModule(
      input.getName().nn,
      name,
      Files.readString(input.toPath(), Charset.defaultCharset()).nn
    )
}
