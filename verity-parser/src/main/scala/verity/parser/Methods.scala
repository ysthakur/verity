package verity.parser

import verity.ast.*
import verity.parser.Core.*
import verity.parser.Exprs.*
import verity.parser.Types.*
import verity.parser.VerityParser.tr

import cats.parse.{Parser as P, Parser0 as P0}

import collection.mutable.ArrayBuffer

/** Parsers for function-related stuff
  */
object Functions {

  private val param: P[ValParam] =
    (identifierText <* ws <* P.char(':') <* ws *> typ).map { case (name, typ) =>
      ValParam(typ, name)
    }

  /** Helper to make parsers for different kinds of parameter lists */
  def paramList(start: P0[Unit], kind: ParamListKind): P[ValParamList] =
    P.defer(
      param.repSep0(ws ~ P.char(',') ~ ws).between(P.char('(') *> start *> ws, ws >* P.char(')'))
    ).map { case params =>
      ValParamList(params, kind)
    }

  val normParamList = paramList(P.unit, ParamListKind.Normal)

  val givenParamList = paramList(identifier("given"), ParamListKind.Given)

  val proofParamList = paramList(identifier("proof"), ParamListKind.Proof)

  val provenClause: P[Seq[Type]] =
    (
      identifier("proof") ~~ !CharPred(
        _.isUnicodeIdentifierPart
      ) ~/ typeRef ~ ("," ~ typeRef).rep
    ).map { case (firstType, restTypes) =>
      firstType +: restTypes
    }

  def letDecl: P[LocalVar] =
    (
      identifier(
        "let"
      ) ~ identifierText ~ P.char(':') ~ typ ~ P.char('=') ~ expr ~ P.index
    ).map { case (name, mods, typ, expr, end) =>
      LocalVar(name, mods, typ, expr, end)
    }
  
  def fnDecl: P[Lambda] =
    ((P.index <* identifier("fn") <* ws).with1
      ~ identifier
      ~ (typeParamList.? <* ws)
      ~ (normParamList.? <* ws)
      ~ (givenParamList.? <* ws)
      ~ (proofParamList.? <* ws)
      ~ (P.string("=") *> expr)
      ~ P.index).map {
      case (start -> typeParams -> normParams -> givenParams -> proofParams -> body -> end) =>
        Lambda(
          typeParams,
          normParams,
          givenParams,
          proofParams,
          body,
          tr(start, end)
        )
    }

  def lambda: P[Lambda] =
    ((P.index <* identifier("fn") <* ws).with1
      ~ (typeParamList.? <* ws)
      ~ (normParamList.? <* ws)
      ~ (givenParamList.? <* ws)
      ~ (proofParamList.? <* ws)
      ~ (P.string("->") *> expr)
      ~ P.index).map {
      case (start -> typeParams -> normParams -> givenParams -> proofParams -> body -> end) =>
        Lambda(
          typeParams,
          normParams,
          givenParams,
          proofParams,
          body,
          tr(start, end)
        )
    }
}
