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
      identifier("val") *> ws *> (identifier ~ (ws *> P.char(
        ':'
      ) *> typ) ~ (ws *> P.char('=') *> expr).?)
    ).map { case (start, name -> typ -> value, end) =>
      Prop(name, typ, value)
    }

  // todo do supertypes and traits
  val classOrTrait: P[TypeDef] =
    (identifier("class") *> identifier.between(ws, ws ~ P.char('=') ~ ws) ~ prop
      .repSep0(ws) <* ws <* identifier("end")).map { case (name, props) =>
      VClass(name, Nil, props, false)
    }
}
