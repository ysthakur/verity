package verity.parser

import verity.ast._
import infile._
import unresolved._
import Core._
import Exprs._
import Types._

import fastparse._
import JavaWhitespace._

import collection.mutable.ArrayBuffer

private object Methods {
  private def param[_: P]: P[Parameter] = P(nonWildcardType ~ identifierText).map { case (typ, name) =>
    Parameter(
      List.empty, //TODO Add annotations!
      typ,
      name,
      isGiven = false,
      isProof = false
    )
  }

  private def params[_: P]: P[List[Parameter]] = P((param ~ ("," ~/ param).rep).?).map {
    case None => Nil
    case Some((firstParam, otherParams)) => firstParam :: otherParams.toList
  }
  
  def normParamList[_: P]: P[ParamList] = P(Index ~ "(" ~/ params ~ ")" ~ Index).map {
    case (start, params, end) =>
      ParamList(params, TextRange(start, end), ParamListKind.NORMAL)
  }
  def givenParamList[_: P]: P[ParamList] =
    P(Index ~ "(" ~ "given" ~ !CharPred(!_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, TextRange(start, end), ParamListKind.GIVEN)
    }
  def proofParamList[_: P]: P[ParamList] =
    P(Index ~ "(" ~ "proof" ~ !CharPred(_.isUnicodeIdentifierPart) ~/ params ~ ")" ~ Index).map {
      case (start, params, end) =>
        ParamList(params, TextRange(start, end), ParamListKind.PROOF)
    }

  //todo allow final modifier
  def localVarDecl[_: P]: P[LocalVar] =
    P(modifiers ~ nonWildcardType ~ Index ~ identifier ~/ Index ~ ("=" ~/ expr).? ~ ";" ~ Index)
      .map { case (mods, typ, idStart, name, idEnd, expr, end) =>
        new LocalVar(mods, Text(name, TextRange(idStart, idEnd)), typ, expr, end)
      }

  def exprStmt[_: P]: P[Statement] = P(expr ~ ";").map {
    case expr => new UnresolvedExprStmt(expr)
  }

  def returnStmt[_: P]: P[ReturnStmt] = P("return" ~/ Index ~ expr ~ ";" ~ Index).map {
    case (start, expr, end) =>
      new ReturnStmt(expr, TextRange(start - 6, end))
  }

  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(returnStmt | localVarDecl | exprStmt)

  def block[_: P]: P[Block] = P("{" ~/ Index ~ stmt.rep ~ "}" ~ Index).map {
    case (start, stmts, end) =>
      new Block(
        stmts.to(ArrayBuffer),
        TextRange(start, end),
        ToBeInferred(UnknownType, UnknownType, Nil)
      )
  }

  //TODO add type parameters
  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeParamList.? ~ (returnTypeAndProofs) ~ identifierText ~ normParamList ~ givenParamList.? ~ proofParamList.? ~ (block | ";" ~ Index))
      .map { case (modifiers, typeParams, (returnType, proofs), name, valParams, givenParams, proofParams, body) =>
        createNormMethod(modifiers, typeParams, returnType, proofs, name, valParams, givenParams, proofParams, body)
      }

  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def ctor[_: P]: P[Seq[Modifier] => (() => HasCtors) => Constructor] =
    P(identifierWithTextRange ~ &("(") ~/ normParamList ~ givenParamList.? ~ proofParamList.? ~ block).map {
      case (name, nameRange, valParams, givenParams, proofParams, body) =>
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
    P(typeParamList ~/ (returnTypeAndProofs) ~ identifierText ~ normParamList ~ givenParamList.? ~ proofParamList.? ~ (block | ";" ~ Index)).map {
      case (typeParams, (returnType, proofs), name, valParams, givenParams, proofParams, body) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(modifiers, Some(typeParams), returnType, proofs, name, valParams, givenParams, proofParams, body)
    }

  def methodWithoutProofsOrTypeParams[_: P]: P[(Type, Text) => Seq[Modifier] => NormMethod] =
    P(normParamList ~ givenParamList.? ~ proofParamList.? ~ (block | ";" ~ Index)).map { case (valParams, givenParams, proofParams, body) =>
      (returnType, name) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(modifiers, None, returnType, Seq.empty, name, valParams, givenParams, proofParams, body)
    }
  def methodWithProofsWithoutTypeParams[_: P]: P[Seq[Modifier] => NormMethod] =
    P(returnTypeAndProofs ~ identifierText ~ normParamList ~ givenParamList.? ~ proofParamList.? ~ (block | ";" ~ Index)).map {
      case (returnType, proofs, name, valParams, givenParams, proofParams, body) =>
        (modifiers: Seq[Modifier]) =>
          createNormMethod(modifiers, None, returnType, proofs, name, valParams, givenParams, proofParams, body)
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
      proofs.toArray,
      name,
      params,
      givenParams, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
      proofParams,
      Nil,
      body
    )
  }

  def returnTypeAndProofs[_: P]: P[(Type, Seq[Type])] =
    P(returnType).map {
      case retType: Type => (retType, Seq())
      case retTypeWithProofs => retTypeWithProofs.asInstanceOf[(Type, Seq[Type])]
    }

  def returnTypeWithProofs[_: P]: P[(Type, Seq[Type])] =
    P("(" ~/ returnType ~ ";" ~ "proof" ~ nonWildcardType ~  ("," ~ nonWildcardType).rep ~ ")").map {
      case (retType, firstProof, restProofs) =>
        (retType, firstProof +: restProofs)
    }
}
