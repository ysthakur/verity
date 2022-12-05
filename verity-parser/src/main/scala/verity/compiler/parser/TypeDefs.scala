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

  val prop: P[Prop] =
    withRange(
      identifier("val") *> identifier.surroundedBy(ws)
        ~ (P.char(':') *> ws *> typ).?
        ~ (P.char('=') *> ws *> expr).?
    ).map { case (start, name -> typ -> value, end) =>
      Prop(name, typ.getOrElse(UnknownType), value)
    }
  
  val enumCase: P[EnumCase] =
    (identifier.surroundedBy(ws) ~ normParamList).map {
      case name => EnumCase(name)
    }

  val enumDef: P[TypeDef] =
    (
      identifier("enum")
        *> identifier.surroundedBy(ws)
        ~ typeParamList.?
        ~ enumCase.rep0
        <* ws <* identifier("end")
    ).map { case (name, props, cases) =>
      EnumDef(name, Nil, props)
    }
}
