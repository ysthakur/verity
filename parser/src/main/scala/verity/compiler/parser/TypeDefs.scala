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
    (identifier("record")
      *> identifier.surroundedBy(ws)
      ~ comptimeParamList.rep0
      ~ valParamList.rep0).map { case (name -> comptimeParamss -> valParamss) =>
      Record(name, comptimeParamss, valParamss)
    }

  val enumCase: P[EnumCase] =
    (identifier.surroundedBy(ws)
      ~ (comptimeParams <* ws)
      ~ (params <* ws)
      ~ (P.char(':') *> ws *> comptimeArgs ~ valArgs).?).map {
      case (name -> comptimeParams -> valParams -> args) =>
        args match {
          case Some((comptimeArgs, valArgss)) =>
            EnumCase(name, comptimeParams, valParams, comptimeArgs, valArgss)
          case None => EnumCase(name, comptimeParams, valParams, Nil, Nil)
        }
    }

  val enumDef: P[TypeDef] =
    (
      identifier("enum")
        *> identifier.surroundedBy(ws)
        ~ comptimeParams0 ~ params0
        ~ enumCase.repSep0(ws *> P.char(',') *> ws).surroundedBy(ws)
        <* identifier("end")
    ).map { case (name -> comptimeParams -> params -> cases) =>
      EnumDef(name, comptimeParams, params, cases)
    }

  val typeAlias: P[TypeDef] =
    (identifier("type")
      *> identifier.surroundedBy(ws)
      ~ (comptimeParams <* ws <* P.char('=') <* ws)
      ~ typ).map { case (name -> params -> body) =>
      TypeAlias(name, params, body)
    }

  val typeDef: P[TypeDef] = recordDef | enumDef
}
