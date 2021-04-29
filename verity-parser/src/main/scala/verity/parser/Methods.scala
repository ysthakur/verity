package verity.parsing.parser

import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import scala.collection.mutable._

private object Methods {
  def param[_: P] = P(typeRef ~ Index ~ identifier ~ Index).map {
    case (typ, nameStart, name, nameEnd) =>
      Parameter(
          List.empty, //TODO Add annotations!
          typ,
          name,
          false,
          false,
          TextRange(typ.textRange.start, nameEnd)
      )
  }
  //TODO make separate givenParamList pattern
  def paramList[_: P] = P(Index ~ "(" ~/ param.rep ~ ")" ~ Index).map { case (start, params, end) =>
    ParamList(params.toList, TextRange(start, end))
  }

  def exprStmt[_: P] = P(expr ~ ";" ~ Index).map { case (expr, end) =>
    new ExprStmt(expr, end)
  }

  //todo allow final modifier
  def localVars[_: P] = P(typeRef ~ identifier ~ ("=" ~ expr).? ~ ";" ~ Index).map {
    case (typ, name, expr, end) => new LocalVar(name, typ, expr, false, end)
  }

  def returnStmt[_: P] = P(Index ~ "return" ~/ expr ~ ";" ~ Index).map { case (start, expr, end) =>
    new ReturnStmt(expr, TextRange(start, end))
  }

  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(exprStmt | localVars)

  def block[_: P]: P[Block] = P(Index ~ "{" ~ (stmt ~ ";".rep).rep ~ "}" ~ Index).map {
    case (start, stmts, end) => Block(stmts.to(ListBuffer), TextRange(start, end))
  }

  //TODO add type parameters
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeRef ~ identifier ~ paramList ~ (block | ";" ~ Index)).map {
      case (modifiers, returnType, name, params, body) =>
        new Method(
            modifiers.to(ListBuffer),
            returnType,
            name,
            params,
            body match {
              case b: Block => Some(b)
              case _        => None
            },
            false,
            TextRange(
                (if (modifiers.isEmpty) returnType else modifiers.head).textRange.start,
                body match {
                  case b: Block => b.textRange.end
                  case e: Int   => e
                }
            )
        )
    }

  /** A constructor
    */
  def ctor[_: P]: P[Method] =
    P(Index ~ modifiers ~ identifier ~ paramList ~ block).map {
      case (start, modifiers, name, params, body) =>
        new Method(
            modifiers.to(ListBuffer),
            ToBeInferred(ObjectType, NothingType, Nil),
            name,
            params,
            Some(body),
            false,
            TextRange(start, body.textRange.end)
        )
    }

  /** A normal method or a constructor
    */
  def method[_: P]: P[Method] = P(ctor | normMethod)
}
