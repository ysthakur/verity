package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.Core.*
import verity.compiler.parser.Exprs.*
import verity.compiler.parser.Types.*
import verity.compiler.parser.Parser.tr

import cats.parse.{Parser as P, Parser0 as P0}

import collection.mutable.ArrayBuffer

/** Parsers for stuff inside TypeDefs
  */
private[parser] object TypeDefs {
  val recordDef: P[TypeDef] =
    (keyword("record") *> ws
      *> (identifier <* ws)
      ~ (comptimeParams0 <* ws)
      ~ params0).map { case (name -> comptimeParamss -> valParamss) =>
      Record(name, comptimeParamss, valParamss)
    }

  val enumCase: P[EnumCase] =
    (identifier.surroundedBy(ws)
      ~ (comptimeParams <* ws)
      ~ (params <* ws)
      ~ (P.char(':') *> ws *> valArgs.eitherOr(comptimeArgs ~~ valArgs)).?).map {
      case (name -> comptimeParams -> valParams -> ctorArgs) =>
        ctorArgs match {
          case Some(Right(valArgs)) =>
            EnumCase(name, comptimeParams, valParams, ComptimeArgs.empty, valArgs)
          case Some(Left((comptimeArgs, valArgs))) =>
            EnumCase(name, comptimeParams, valParams, comptimeArgs, valArgs)
          case None => EnumCase(name, comptimeParams, valParams, ComptimeArgs.empty, Args.empty)
        }
    }

  val enumDef: P[TypeDef] =
    (
      keyword("enum")
        *> identifier.surroundedBy(ws)
        ~ comptimeParams0 ~ params0
        ~ enumCase.repSep0(ws *> P.char(',') *> ws).surroundedBy(ws)
        <* keyword("end")
    ).map { case (name -> comptimeParams -> params -> cases) =>
      EnumDef(name, comptimeParams, params, cases)
    }

  val typeAlias: P[TypeDef] =
    (keyword("type")
      *> identifier.surroundedBy(ws)
      ~ (comptimeParams <* ws <* P.char('=') <* ws)
      ~ typ).map { case (name -> params -> body) =>
      TypeAlias(name, params, body)
    }

  val typeDef: P[TypeDef] = recordDef | enumDef
}
