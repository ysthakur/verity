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

private class Methods(core: Core, types: Types, exprs: Exprs)(implicit offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]) {
  import core._
  import types._
  import exprs._


  // private def returnTypeOrProofs[_: P]: P[(Type, Seq[Type])] =
  //   P(returnType).map {
  //     case retType: Type     => (retType, Seq())
  //     case retTypeWithProofs => retTypeWithProofs.asInstanceOf[(Type, Seq[Type])]
  //   }

  def returnTypeWithProofs[_: P]: P[(Type, Seq[Type])] =
    P("(" ~/ returnType ~ ";" ~ "proof" ~ nonWildcardType ~ ("," ~ nonWildcardType).rep ~ ")").map {
      case (retType, firstProof, restProofs) =>
        (retType, firstProof +: restProofs)
    }

  private def param[_: P]: P[Parameter] = P(nonWildcardType ~ identifierText).map {
    case (typ, name) =>
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
    P("(" ~ Index ~ "given" ~ !CharPred(!_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.GIVEN)
    }
  def proofParamList[_: P]: P[ParamList] =
    P("(" ~ Index ~ "proof" ~ !CharPred(!_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, ps2tr(start, end), ParamListKind.PROOF)
    }

  def proofClause[_: P]: P[Seq[Type]] =
    P(("proof" ~/ typeRef ~ ("," ~ typeRef).rep).?).map {
      case None => Nil
      case Some((firstType, restTypes)) => firstType +: restTypes
    }

  //todo allow final modifier
  def localVarDecl[_: P]: P[LocalVar] =
    P(modifiers ~ nonWildcardType ~ Index ~ identifier ~/ Index ~ ("=" ~/ expr).? ~ ";" ~ Index)
      .map { case (mods, typ, idStart, name, idEnd, expr, end) =>
        new LocalVar(mods, Text(name, ps2tr(idStart, idEnd)), typ, expr, end)
      }

  def exprStmt[_: P]: P[Statement] = P(expr ~ ";").map { case expr =>
    new UnresolvedExprStmt(expr)
  }

  def returnStmt[_: P]: P[ReturnStmt] = P("return" ~/ Index ~ expr ~ ";" ~ Index).map {
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

  def paramsAndBlock[_: P]: P[(ParamList, Option[ParamList], Option[ParamList], Seq[Type], Any)] =
    P(normParamList ~ givenParamList.? ~ proofParamList.? ~ proofClause ~ (block | ";" ~ Index))

  //TODO add type parameters
  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeParamList.? ~ returnType ~ identifierText ~ paramsAndBlock).map {
      case (
            modifiers,
            typeParams,
            returnType,
            name,
            (valParams, givenParams, proofParams, provenProofs, body)
          ) =>
        createNormMethod(
          modifiers,
          typeParams,
          returnType,
          provenProofs,
          name,
          valParams,
          givenParams,
          proofParams,
          body
        )
    }

  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def ctor[_: P]: P[Seq[Modifier] => (() => HasCtors) => Constructor] =
    P(
      identifierWithTextRange ~ &(
        "("
      ) ~/ normParamList ~ givenParamList.? ~ proofParamList.? ~ block
    ).map { case (name, nameRange, valParams, givenParams, proofParams, body) =>
      modifiers =>
        cls =>
          new Constructor(
            modifiers.to(ArrayBuffer),
            name,
            nameRange,
            valParams,
            givenParams,
            proofParams,
            Nil,
            body,
            cls
          )
    }

  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def methodWithTypeParams[_: P]: P[Seq[Modifier] => NormMethod] =
    P(typeParamList ~/ returnType ~ identifierText ~ paramsAndBlock).map {
      case (typeParams, returnType, name, (valParams, givenParams, proofParams, provenProofs, body)) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(
            modifiers,
            Some(typeParams),
            returnType,
            provenProofs,
            name,
            valParams,
            givenParams,
            proofParams,
            body
          )
    }

  def methodWithoutProofsOrTypeParams[_: P]: P[(Type, Text) => Seq[Modifier] => NormMethod] =
    P(paramsAndBlock).map { case (valParams, givenParams, proofParams, provenProofs, body) =>
      (returnType, name) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(
            modifiers,
            None,
            returnType,
            provenProofs,
            name,
            valParams,
            givenParams,
            proofParams,
            body
          )
    }
  def methodWithProofsWithoutTypeParams[_: P]: P[Seq[Modifier] => NormMethod] =
    P(returnType ~ identifierText ~ paramsAndBlock).map {
      case (returnType, name, (valParams, givenParams, proofParams, provenProofs, body)) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(
            modifiers,
            None,
            returnType,
            provenProofs,
            name,
            valParams,
            givenParams,
            proofParams,
            body
          )
    }

  def createNormMethod(
    modifiers: Seq[Modifier],
    typeParams: Option[TypeParamList],
    returnType: Type,
    proofs: Seq[Type],
    name: Text,
    params: ParamList,
    givenParams: Option[ParamList],
    proofParams: Option[ParamList],
    bodyOrEnd: Any
  ): NormMethod = {
    val (body, end) = (bodyOrEnd: @unchecked) match {
      case b: Block => Some(b) -> b.textRange.end
      case e: Int   => None -> e
    }
    new NormMethod(
      modifiers.to(ArrayBuffer),
      typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
      returnType,
      proofs,
      name,
      params,
      givenParams, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
      proofParams,
      Nil,
      body
    )
  }
}
