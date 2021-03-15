package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import collection.mutable._

private object Methods {
  def param[_ : P] = P(typeRef ~ identifier).map { 
    case (typ, name) => Parameter(
      List.empty[Annotation], //TODO Add annotations!
      typ,
      name,
      false,
      false,
      TextRange(typ.textRange.start, name.textRange.end)
    )
  }
  //TODO make separate givenParamList pattern
  def paramList[_ : P] = P(Index ~ "(" ~ param.rep ~ ")" ~ Index).map { 
    case (start, params, end) => ParamList(params.toList, TextRange(start, end))
  }

  def exprStmt[_ : P] = P(expr ~ ";" ~ Index ~ ";".rep).map {
    case (expr, end) => new ExprStmt(expr, end)
  }
  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_ : P]: P[Statement] = P(exprStmt)

  def methodBody[_ : P]: P[Block] = P(Index ~ "{" ~ stmt.rep ~ "}" ~ Index).map {
    case (start, stmts, end) => Block(ListBuffer(stmts: _*), TextRange(start, end))
  }

  
  def normMethod[_ : P]: P[Method] =
    P(modifiers ~ typeRef ~ identifier ~ paramList ~ (methodBody | ";" ~ Index)).map {
      case (modifiers, returnType, name, params, body) =>
        Method(
          ListBuffer(modifiers: _*),
          returnType,
          name,
          params,
          Some(body.asInstanceOf[Block]), //todo don't just cast for abstract methods
          false
        )
    }

  
  /**
    * A normal method or a constructor
    */
  def method[_: P]: P[Method] = 
    P(modifiers ~ identifier ~ (typeArgList.? ~ identifier).? ~ paramList ~ (methodBody | ";" ~ Index)).map {
      case foo => ???
    }
}