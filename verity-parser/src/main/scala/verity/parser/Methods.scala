package verity.parser

import verity.ast._
import infile._
import unresolved._
// import Core._
// import Exprs._
// import Types._
import Parser.ps2tr

import fastparse._
import JavaWhitespace._

import collection.mutable.ArrayBuffer

private class Methods(core: Core, types: Types, exprs: Exprs)(implicit
  offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]
) {
  import core._
  import types._
  import exprs._

  private def param[_: Parser]: Parser[Parameter] =
    Parser(identifierText ~ ":" ~ nonWildcardType).map { case (name, typ) =>
      Parameter(
        List.empty, // TODO Add annotations!
        typ,
        name,
        isGiven = false,
        isProof = false
      )
    }

  private def params[_: Parser]: Parser[List[Parameter]] =
    Parser((param ~ ("," ~/ param).rep).?).map {
      case None => Nil
      case Some((firstParam, otherParams)) => firstParam :: otherParams.toList
    }

  def normParamList[_: Parser]: Parser[ParamList] = Parser("(" ~ Index ~ params ~ ")" ~ Index).map {
    case (start, params, end) =>
      ParamList(params, ps2tr(start, end), ParamListKind.NORMAL)
  }
  def givenParamList[_: Parser]: Parser[ParamList] =
    Parser("(" ~ Index ~ "given" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index)
      .map { case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.GIVEN)
      }
  def proofParamList[_: Parser]: Parser[ParamList] =
    Parser("(" ~ Index ~ "proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index)
      .map { case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.PROOF)
      }

  def proofClause[_: Parser]: Parser[Seq[Type]] =
    Parser("proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ typeRef ~ ("," ~ typeRef).rep).map {
      case (firstType, restTypes) => firstType +: restTypes
    }

  // todo allow final modifier
  def letDecl[_: Parser]: Parser[LocalVar] =
    Parser(identifier("let") ~ modifiers ~ identifierText ~ ":" ~ typeRef ~ "=" ~ expr ~ Index)
      .map { case (name, mods, typ, expr, end) =>
        new LocalVar(name, mods, typ, expr, end)
      }

  def lambda: Parser[Lambda] = (
    Parser.char('{') ~ Index
      ~ typeParamList.?
      ~ normParamList
      ~ givenParamList.?
      ~ proofParamList.?
      ~ Parser.char('}') ~ Index
  ).map { case (start, expr, end) =>
    new Lambda(expr, ps2tr(start, end))
  }
}
