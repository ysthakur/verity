package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.Core.*
import verity.compiler.parser.Exprs.expr
import verity.compiler.parser.TypeDefs.typeDef
import verity.compiler.parser.Types.typ
import verity.compiler.Message
import verity.compiler.Result
import verity.compiler.Source

import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path

import cats.data.Chain
import cats.data.Writer
import cats.implicits.toShow
import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.LocationMap

import collection.mutable.ArrayBuffer

object Parser {
  private val globalVar: P[GlobalVar] =
    (
      keyword("let") *> identifier.surroundedBy(ws)
        ~ (P.char(':') *> ws *> typ <* ws).?
        ~ (P.char('=') *> ws *> expr <* ws)
    ).map { case (varName -> typ -> value) =>
      GlobalVar(varName, typ.getOrElse(ToBeInferred), value)
    }

  private val module: P[ModuleDef] =
    (ws.with1 *> keyword("mod") *> ws *> identifier
      ~ P.defer0(moduleContents: @unchecked) <* keyword("end") <* ws).map {
      case (name, toModule) => toModule(name, None)
    }

  /** Parse the contents of a module but not the surrounding mod/end bit.
    * Returns a function that takes the module name and its file (if the
    * module's the entire file) and constructs the module.
    */
  private lazy val moduleContents: P0[(String, Option[File]) => ModuleDef] =
    (ws *> (importStmt.repSep0(ws) <* ws) ~ module
      .eitherOr(typeDef.eitherOr(globalVar))
      .repSep0(ws) <* ws)
      .map { case (imports, contents) =>
        (name, file) => {
          val (rest, modules) = contents.partitionMap(Predef.identity)
          val (globalVars, typeDefs) = rest.partitionMap(Predef.identity)
          SourceModule(name, imports, modules, typeDefs, globalVars, file)
        }
      }

  private def processResult[T](
      src: Source,
      parserResult: Either[P.Error, T],
  ): Result[Option[T]] =
    (parserResult: @unchecked) match {
      case Right(ast) => Result.some(ast)
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
                  Pos(caret.offset, caret.line, caret.col + 1),
                )
            }
        }
        Result.msg(
          Message.err(
            s"Expected one of ${error.expected.toList.map(e => s"\n- ${e.show}").mkString}",
            span,
            src,
          ),
        )
    }

  /** Parse a module from a string */
  def parseModule(code: String): Result[Option[ModuleDef]] =
    processResult(Source.Str(code), module.parseAll(code))

  /** Like [[parseModule]], but the code is only the contents of the module (no
    * module name and end marker)
    */
  def parseModuleContents(
      moduleName: String,
      code: String,
  ): Result[Option[ModuleDef]] =
    processResult(
      Source.Str(code),
      moduleContents
        .map { toModule => toModule(moduleName, None) }
        .parseAll(code),
    )

  def parseFile(moduleName: String, input: File): Result[Option[ModuleDef]] = {
    val code = Files.readString(input.toPath(), Charset.defaultCharset()).nn
    processResult(
      Source.File(input),
      moduleContents
        .map { toModule => toModule(moduleName, Some(input)) }
        .parseAll(code),
    )
  }
}
