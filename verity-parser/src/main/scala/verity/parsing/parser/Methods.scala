package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import collection.mutable._

private object Methods {
  def param[_: P] = P(typeRef ~ identifier).map {
    case (typ, name) =>
      Parameter(
          List.empty, //TODO Add annotations!
          typ,
          name,
          false,
          false,
          TextRange(typ.textRange.start, name.textRange.end)
      )
  }
  //TODO make separate givenParamList pattern
  def paramList[_: P] = P(Index ~ "(" ~/ param.rep ~ ")" ~ Index).map {
    case (start, params, end) => ParamList(params.toList, TextRange(start, end))
  }

  def exprStmt[_: P] = P(expr ~ ";" ~ Index).map {
    case (expr, end) => new ExprStmt(expr, end)
  }
  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(exprStmt)

  def methodBody[_: P]: P[Block] = P(Index ~ "{" ~ (stmt ~ ";".rep).rep ~ "}" ~ Index).map {
    case (start, stmts, end) => Block(stmts.to(ListBuffer), TextRange(start, end))
  }

  //TODO add type parameters
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeRef ~ identifier ~ paramList ~ (methodBody | ";" ~ Index)).map {
      case (modifiers, returnType, name, params, body) =>
        Method(
            modifiers.to(ListBuffer),
            returnType,
            name,
            params,
            body match {
              case b: Block => Some(b)
              case _        => None
            },
            false
        )
    }

  /**
    * A constructor
    */
  def ctor[_: P]: P[Method] =
    P(modifiers ~ identifier ~ paramList ~ (methodBody | ";" ~ Index)).map {
      case (modifiers, name, params, body) =>
        Method(
            modifiers.to(ListBuffer),
            ToBeInferred(ObjectType, NothingType, Nil),
            name,
            params,
            body match {
              case b: Block => Some(b)
              case _        => None
            },
            false
        )
    }

  /**
    * A normal method or a constructor
    */
  def method[_: P]: P[Method] = P(ctor | normMethod)
}
