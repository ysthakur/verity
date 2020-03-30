package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar._
import com.ysthakur.parsing.lexer.SymbolTokenTypes._
import com.ysthakur.parsing.lexer.{Token, TokenType, ValidIdentifierTokenType}

import scala.language.postfixOps

object ParserPatterns {
  type P = Pattern[Node]
  implicit def c(m: SingleMatch[Node]): Node = {
    m.matched
  }
  val validId: P = FunctionPattern[Node]((input: Iterable[Node]) =>
    ifSingleTokenWithType(input) { _.isInstanceOf[ValidIdentifierTokenType] }
  ).>>[Node]
  val dotReference: P     = validId - ((DOT - validId) *)
  lazy val unaryPreOp: P  = PLUSX2 | MINUSX2
  lazy val unaryPostOp: P = PLUSX2 | MINUSX2
  lazy val unaryExpr: P   = (expr - unaryPostOp) | (unaryPreOp - expr)
  lazy val expr: P        = validId

  def ifSingleToken(
      input: Iterable[Node]
  )(f: Node => MatchResult): MatchResult =
    if (input.size == 1) f(input.head)
    else NoMatch()
  def ifSingleTokenWithType(
      input: Iterable[Node]
  )(f: TokenType => Boolean): MatchResult = {
    ifSingleToken(input) {
      case token: Token =>
        if (f(token.tokenType))
          FullMatch(SingleMatch(token, 0), couldMatchMore = false)
        else NoMatch()
      case _ => NoMatch()
    }
  }

}
