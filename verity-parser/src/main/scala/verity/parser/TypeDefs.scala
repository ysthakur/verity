package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.Core.*
import verity.compiler.parser.Exprs.*
import verity.compiler.parser.Types.*
import verity.compiler.parser.VerityParser.tr

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
