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
      ~ constParamList.rep0
      ~ valParamList.rep0).map { case (name -> constParamss -> valParamss) =>
      Record(name, constParamss, valParamss)
    }

  val enumCase: P[EnumCase] =
    (identifier.surroundedBy(
      ws
    ) ~ constParamList.rep0 ~ valParamList.rep ~ (ws *> P.char(':') *> ws *> constArgList ~ valArgList).?).map {
      case (name -> constParamss -> valParamss -> args) =>
        args match {
          case None => EnumCase(name, constParamss, valParamss, Nil, Nil)
        }
    }

  val enumDef: P[TypeDef] =
    (
      identifier("enum")
        *> identifier.surroundedBy(ws)
        ~ constParamList.rep0 ~ valParamList.rep0
        ~ enumCase.repSep0(P.char(','))
        <* ws <* identifier("end")
    ).map { case (name -> constParamss -> valParamss -> cases) =>
      EnumDef(name, constParamss, valParamss, cases)
    }

  val typeAlias: P[TypeDef] =
    (identifier("type")
      *> identifier.surroundedBy(ws)
      ~ (constParamList.rep0 <* ws <* P.char('=') <* ws)
      ~ typ).map {
    case (name -> paramss -> body) => TypeAlias(name, paramss, body)
  }

  val typeDef: P[TypeDef] = recordDef | enumDef
}
