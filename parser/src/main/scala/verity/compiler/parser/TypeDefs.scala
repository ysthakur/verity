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
      ~ comptimeParamList.rep0
      ~ (params <* ws)
      ~ (P.char(':') *> ws *> comptimeArgList.rep0 ~ valArgList.rep0).?).map {
      case (name -> comptimeParamss -> (normParams, givenParams) -> args) =>
        args match {
          case Some((comptimeArgss, valArgss)) =>
            EnumCase(name, comptimeParamss, normParams, givenParams, comptimeArgss, valArgss)
          case None => EnumCase(name, comptimeParamss, normParams, givenParams, Nil, Nil)
        }
    }

  val enumDef: P[TypeDef] =
    (
      identifier("enum")
        *> identifier.surroundedBy(ws)
        ~ comptimeParamList.repSep0(ws) ~ normParamList.surroundedBy(ws).? ~ givenParamList.?
        ~ enumCase.repSep0(ws *> P.char(',') *> ws).surroundedBy(ws)
        <* identifier("end")
    ).map { case (name -> comptimeParamss -> normParams -> givenParams -> cases) =>
      EnumDef(name, comptimeParamss, normParams.getOrElse(Nil), givenParams.getOrElse(Nil), cases)
    }

  val typeAlias: P[TypeDef] =
    (identifier("type")
      *> identifier.surroundedBy(ws)
      ~ (comptimeParamList.rep0 <* ws <* P.char('=') <* ws)
      ~ typ).map { case (name -> paramss -> body) =>
      TypeAlias(name, paramss, body)
    }

  val typeDef: P[TypeDef] = recordDef | enumDef
}
