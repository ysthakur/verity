package verity.parser

import verity.ast.*
import verity.parser.Core.*
import verity.parser.Exprs.*
import verity.parser.Types.*
import verity.parser.VerityParser.tr

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

  // todo do supertypes and traits
  val classOrTrait: P[TypeDef] =
    (identifier("class") *> identifier.surroundedBy(ws)
      ~ prop.repSep0(ws)).map { case (name, props) =>
      ClassDef(name, Nil, props, false)
    }
}
