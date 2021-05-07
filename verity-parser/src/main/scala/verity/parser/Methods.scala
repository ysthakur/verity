package verity.parser

import verity.ast._
import infile._
import unresolved._
import Core._
import Exprs._
import Types._

import fastparse._
import JavaWhitespace._

import collection.mutable.ListBuffer

private object Methods {
  def param[_: P]: P[Parameter] = P(nonWildcardType ~ identifierText).map {
    case (typ, name) =>
      Parameter(
          List.empty, //TODO Add annotations!
          typ,
          name,
          isGiven = false,
          isErased = false,
          TextRange(typ.textRange.start, name.textRange.end)
      )
  }
  //TODO make separate givenParamList pattern
  def paramList[_: P]: P[ParamList] = P(Index ~ "(" ~/ param.rep ~ ")" ~ Index).map { case (start, params, end) =>
    ParamList(params.toList, TextRange(start, end))
  }

  def exprStmt[_: P]: P[ExprStmt] = P(expr ~ ";" ~ Index).map { case (expr, end) =>
    new ExprStmt(expr, end)
  }

  //todo allow final modifier
  def localVarDecl[_: P]: P[LocalVar] =
    P(modifiers ~ nonWildcardType ~ Index ~ identifier ~/ Index ~ ("=" ~/ expr).? ~ ";" ~ Index).map {
      case (mods, typ, idStart, name, idEnd, expr, end) =>
        new LocalVar(mods, Text(name, TextRange(idStart, idEnd)), typ, expr, end)
    }

  def returnStmt[_: P]: P[ReturnStmt] = P("return" ~/ Index ~ expr ~ ";" ~ Index).map { case (start, expr, end) =>
    new ReturnStmt(expr, TextRange(start - 6, end))
  }

  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(returnStmt | localVarDecl | exprStmt)

  def block[_: P]: P[UnresolvedBlock] = P(Index ~ "{" ~/ (stmt ~ ";".rep).rep ~ "}" ~ Index).map {
    case (start, stmts, end) => UnresolvedBlock(stmts.to(ListBuffer), TextRange(start, end))
  }

  //TODO add type parameters
  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeParamList.? ~ nonWildcardType ~ identifierText ~ paramList ~ (block | ";" ~ Index)).map {
      case (modifiers, typeParams, returnType, name, params, body) =>
        createUnresolvedNormMethod(modifiers, typeParams, returnType, name, params, None, None, body)
    }

  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def ctor[_: P]: P[Seq[Modifier] => UnresolvedConstructor] =
    P(identifierText ~ &("(") ~/ paramList ~ block).map {
      case (name, params, body) =>
        modifiers => new UnresolvedConstructor(
            modifiers.to(ListBuffer),
            name,
            params,
            None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
            None,
            body,
            TextRange((if (modifiers.isEmpty) name else modifiers.head).textRange.start, body.textRange.end)
        )
    }

  //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
  def methodWithTypeParams[_: P]: P[Seq[Modifier] => UnresolvedNormMethod] =
    P(typeParamList ~/ nonWildcardType ~ identifierText ~ paramList ~ (block | ";" ~ Index)).map {
      case (typeParams, returnType, name, params, body) =>
        (modifiers: Seq[Modifier]) =>
          createUnresolvedNormMethod(modifiers, Some(typeParams), returnType, name, params, None, None, body)
    }

  def methodWithoutTypeParams[_: P]: P[(Type, Text) => Seq[Modifier] => UnresolvedNormMethod] =
    P(paramList ~ (block | ";" ~ Index)).map {
      case (params, body) =>
        (returnType: Type, name: Text) => (modifiers: Seq[Modifier]) =>
          createUnresolvedNormMethod(modifiers, None, returnType, name, params, None, None, body)
    }
    
  def createUnresolvedNormMethod(
      modifiers: Seq[Modifier],
      typeParams: Option[TypeParamList],
      returnType: Type,
      name: Text,
      params: ParamList,
      givenParams: Option[ParamList],
      proofParams: Option[ParamList],
      bodyOrEnd: Any
  ): UnresolvedNormMethod = {
    val (body, end) = (bodyOrEnd: @unchecked) match {
      case b: UnresolvedBlock => Some(b) -> b.textRange.end
      case e: Int => None -> e
    }
    new UnresolvedNormMethod(
      modifiers.to(ListBuffer),
      typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
      returnType,
      name,
      params,
      givenParams, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
      proofParams,
      body,
      TextRange(
        (if (modifiers.isEmpty) returnType else modifiers.head).textRange.start,
        end
      )
    )
  }
}
