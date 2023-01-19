package verity.compiler.parser

import verity.compiler.ast.{ModuleMember, ModuleDef, TextRange, Position}
import verity.compiler.parser.Core.*

import cats.parse.{Parser => CatsParser, Parser0}

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.Charset
import collection.mutable.ArrayBuffer
import verity.compiler.ast.ImportStmt
import verity.compiler.ast.VarDef
import verity.compiler.ast.TypeDef

case class SyntaxError(message: String, location: Int)

object Parser {
  private val module: CatsParser[ModuleDef] =
    (keyword("mod") *> ws
      *> identifier
      ~ CatsParser.defer0(moduleContents: @unchecked)
      <* keyword("end").?).map { case (name, contents) =>
      ModuleDef(name, contents)
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
        Left(SyntaxError(s"Expected $expected", failedAtOffset))
    }

  def parseModule(
    moduleName: String,
    code: String
  ): Either[SyntaxError, ModuleDef] =
    processResult(
      moduleContents
        .map { contents =>
          ModuleDef(moduleName, contents)
        }
        .parseAll(code)
    )

  def parseFile(name: String, input: File): Either[SyntaxError, ModuleDef] =
    parseModule(
      name,
      Files.readString(input.toPath(), Charset.defaultCharset()).nn
    )

  /** Just a shorter way to create TextRanges */
  private[parser] def tr(startOffset: Int, endOffset: Int) =
    TextRange(startOffset, endOffset)
}
