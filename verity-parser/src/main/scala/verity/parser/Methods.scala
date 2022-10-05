package verity.parser

import verity.ast._
// import Core._
// import Exprs._
// import Types._
import VerityParser.tr

import cats.parse.{Parser as P, Parser0 as P0}

import collection.mutable.ArrayBuffer

private class Methods(core: Core, types: Types, exprs: Exprs) {
  import core._
  import types._
  import exprs._

  private def param: P[Parameter] =
    (identifierText ~ ":" ~ nonWildcardType).map { case (name, typ) =>
      Parameter(
        List.empty, // TODO Add annotations!
        typ,
        name,
        isGiven = false,
        isProof = false
      )
    }

  private def params: P[List[Parameter]] =
    P((param ~ ("," ~/ param).rep).?).map {
      case None                            => Nil
      case Some((firstParam, otherParams)) => firstParam :: otherParams.toList
    }

  def normParamList: P[ParamList] =
    P("(" ~ P.index ~ params ~ ")" ~ P.index).map { case (start, params, end) =>
      ParamList(params, tr(start, end), ParamListKind.NORMAL)
    }
  def givenParamList: P[ParamList] =
    P(
      "(" ~ P.index ~ "given" ~~ !CharPred(
        _.isUnicodeIdentifierPart
      ) ~/ params ~ ")" ~ P.index
    ).map { case (start, params, end) =>
      ParamList(params, tr(start, end), ParamListKind.GIVEN)
    }
  def proofParamList: P[ParamList] =
    P(
      "(" ~ P.index ~ "proof" ~~ !CharPred(
        _.isUnicodeIdentifierPart
      ) ~/ params ~ ")" ~ P.index
    ).map { case (start, params, end) =>
      ParamList(params, tr(start, end), ParamListKind.PROOF)
    }

  def proofClause: P[Seq[Type]] =
    P(
      "proof" ~~ !CharPred(
        _.isUnicodeIdentifierPart
      ) ~/ typeRef ~ ("," ~ typeRef).rep
    ).map { case (firstType, restTypes) =>
      firstType +: restTypes
    }

  // todo allow final modifier
  def letDecl: P[LocalVar] =
    (
      identifier(
        "let"
      ) ~ modifiers ~ identifierText ~ ":" ~ typeRef ~ "=" ~ expr ~ P.index
    ).map { case (name, mods, typ, expr, end) =>
      LocalVar(name, mods, typ, expr, end)
    }

  def lambda: P[Lambda] =
    ((P.index <* identifier("fn") <* ws)
      ~ (typeParamList.? <* ws)
      ~ (normParamList.? <* ws)
      ~ (givenParamList.? <* ws)
      ~ (proofParamList.? <* ws)
      ~ (P.string("=>") *> expr)
      ~ P.index).map { case (start -> typeParams -> normParams -> givenParams -> proofParams -> body -> end) =>
      Lambda(typeParams, normParams, givenParams, proofParams, body, tr(start, end))
    }
}
