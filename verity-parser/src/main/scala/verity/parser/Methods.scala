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

  private def param[_: P]: P[Parameter] = P(identifierText ~ ":" ~ nonWildcardType).map {
    case (name, typ) =>
      Parameter(
        List.empty, //TODO Add annotations!
        typ,
        name,
        isGiven = false,
        isProof = false
      )
  }

  private def params[_: P]: P[List[Parameter]] = P((param ~ ("," ~/ param).rep).?).map {
    case None                            => Nil
    case Some((firstParam, otherParams)) => firstParam :: otherParams.toList
  }

  def normParamList[_: P]: P[ParamList] = P("(" ~ Index ~ params ~ ")" ~ Index).map {
    case (start, params, end) =>
      ParamList(params, ps2tr(start, end), ParamListKind.NORMAL)
  }
  def givenParamList[_: P]: P[ParamList] =
    P("(" ~ Index ~ "given" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.GIVEN)
    }
  def proofParamList[_: P]: P[ParamList] =
    P("(" ~ Index ~ "proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.PROOF)
    }

  def proofClause[_: P]: P[Seq[Type]] =
    P("proof" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ typeRef ~ ("," ~ typeRef).rep).map {
      case (firstType, restTypes) => firstType +: restTypes
    }

  /** Matches either "val" or "var", result is whether or not it matched "val" i.e. it's final.
    */
  def valOrVal[_: P]: P[Boolean] =
    P(StringIn("val", "var").! ~~ !CharPred(_.isUnicodeIdentifierPart)).map(_.charAt(2) == 'l')

  //todo allow final modifier
  def localVarDecl[_: P]: P[LocalVar] =
    P(modifiers ~ valOrVal ~/ identifierText ~ ":" ~ typeRef ~ ("=" ~/ expr).? ~ ";" ~ Index).map {
      case (mods, isFinal, name, typ, expr, end) =>
        new LocalVar(mods, name, typ, expr, end, isFinal)
    }

  def exprStmt[_: P]: P[Statement] = P(expr ~ ";").map { case expr =>
    new UnresolvedExprStmt(expr)
  }

  def returnStmt[_: P]: P[ReturnStmt] = P("return" ~/ Index ~ expr.? ~ ";" ~ Index).map {
    case (start, expr, end) =>
      new ReturnStmt(expr, ps2tr(start - 6, end))
  }

  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(returnStmt | localVarDecl | exprStmt)

  def block[_: P]: P[Block] = P("{" ~/ Index ~ stmt.rep ~ "}" ~ Index).map {
    case (start, stmts, end) =>
      new Block(
        stmts.to(ArrayBuffer),
        ps2tr(start, end),
        ToBeInferred(UnknownType, UnknownType, Nil)
      )
  }

  /**
   * Matches a method without its modifiers, then returns a function that takes
   * those modifiers and creates an actual Method object.
   */
  def normMethod[_: P]: P[Seq[Modifier] => NormMethod] =
    P(
      "def" ~~ !CharPred(_.isUnicodeIdentifierStart)
        ~/ identifierText
        ~ typeParamList.? ~ normParamList ~ givenParamList.? ~ proofParamList.?
        ~ ":" ~ returnType
        ~ proofClause.?
        ~ (";" ~ Index | block)
    ).map {
      case (
            name,
            typeParams,
            valParams,
            givenParams,
            proofParams,
            returnType,
            provenProofs,
            bodyOrEnd
          ) =>
        (modifiers: Seq[Modifier]) => {
          val (body, end) = (bodyOrEnd: @unchecked) match {
            case b: Block => Some(b) -> b.textRange.end
            case e: Int   => (None: Option[Block]) -> e
          }
          new NormMethod(
            modifiers.to(ArrayBuffer),
            typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
            returnType,
            provenProofs.getOrElse(Nil),
            name,
            valParams,
            givenParams,
            proofParams,
            Nil,
            body
          )
        }
    }

  /**
   * Matches a constructor without its modifiers, then returns a function that takes
   * those modifiers and creates an actual Constructor object.
   */
  def ctor[_: P]: P[Seq[Modifier] => (() => HasCtors) => Constructor] =
    P(
      "constructor" ~/ normParamList ~ givenParamList.? ~ proofParamList.? ~ block
    ).map { case (valParams, givenParams, proofParams, body) =>
      modifiers =>
        cls =>
          new Constructor(
            modifiers.to(ArrayBuffer),
            valParams,
            givenParams,
            proofParams,
            Nil,
            body,
            cls
          )
    }

}
