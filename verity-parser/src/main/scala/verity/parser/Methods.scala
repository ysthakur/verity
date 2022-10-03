package verity.parser

import verity.ast._
import infile._
import unresolved._
// import Core._
// import Exprs._
// import Types._
import VerityParser.ps2tr

import cats.parse.Parser

import collection.mutable.ArrayBuffer

private class Methods(core: Core, types: Types, exprs: Exprs)(implicit
  offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]
) {
  import core._
  import types._
  import exprs._

  private def param: Parser[Parameter] =
    (identifierText ~ ":" ~ nonWildcardType).map { case (name, typ) =>
      Parameter(
        List.empty, // TODO Add annotations!
        typ,
        name,
        isGiven = false,
        isProof = false
      )
    }

  private def params: Parser[List[Parameter]] =
    Parser((param ~ ("," ~/ param).rep).?).map {
      case None => Nil
      case Some((firstParam, otherParams)) => firstParam :: otherParams.toList
    }

  def normParamList: Parser[ParamList] = Parser("(" ~ Parser.index ~ params ~ ")" ~ Parser.index).map {
    case (start, params, end) =>
      ParamList(params, ps2tr(start, end), ParamListKind.NORMAL)
  }
  def givenParamList: Parser[ParamList] =
    Parser("(" ~ Parser.index ~ "given" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Parser.index)
      .map { case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.GIVEN)
      }
  def proofParamList: Parser[ParamList] =
    Parser("(" ~ Parser.index ~ "proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Parser.index)
      .map { case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.PROOF)
      }

  def proofClause: Parser[Seq[Type]] =
    Parser("proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ typeRef ~ ("," ~ typeRef).rep).map {
      case (firstType, restTypes) => firstType +: restTypes
    }

  // todo allow final modifier
  def letDecl: Parser[LocalVar] =
    Parser(identifier("let") ~ modifiers ~ identifierText ~ ":" ~ typeRef ~ "=" ~ expr ~ Parser.index)
      .map { case (name, mods, typ, expr, end) =>
        new LocalVar(name, mods, typ, expr, end)
      }

  def lambda: Parser[Lambda] = (
    Parser.char('{') ~ Parser.index
      ~ typeParamList.?
      ~ normParamList
      ~ givenParamList.?
      ~ proofParamList.?
      ~ Parser.char('}') ~ Parser.index
  ).map { case (start, expr, end) =>
    new Lambda(expr, ps2tr(start, end))
  }
}
