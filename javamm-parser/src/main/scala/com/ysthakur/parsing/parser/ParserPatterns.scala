package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.infile.ValidIdNode
import com.ysthakur.parsing.ast.infile.expr._
import com.ysthakur.util._
import com.ysthakur.parsing.grammar._
import com.ysthakur.parsing.lexer.SymbolTokenTypes._
import com.ysthakur.parsing.lexer.{Token, TokenType, ValidIdentifierTokenType}

import scala.language.postfixOps

object ParserPatterns {

  type P                   = Pattern[Node]
  type PC[T[_], N <: Node] = PatternWithConstructor[Node, T[Node], N]
  implicit def c(m: SingleMatch[Node]): Node = {
    new Node {}
  }
  lazy val unaryPreOp: P         = PLUSX2 | MINUSX2
  lazy val unaryPostOp: P        = PLUSX2 | MINUSX2
  lazy val unaryExpr: P          = (expr - unaryPostOp) | (unaryPreOp - expr)
  lazy val expr: PC[Match, Expr] = validId.as[Pattern[Node]]
  val validId: PC[SingleMatch, ValidIdNode] =
    FunctionPattern[Node]((input: Iterable[Node]) =>
      ifSingleTokenWithType(input) { _.isInstanceOf[ValidIdentifierTokenType] }
    )
  val dotReference: P = validId - ((DOT - validId) *)

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

  def ifSingleToken(
      input: Iterable[Node]
  )(f: Node => MatchResult): MatchResult =
    if (input.size == 1) f(input.head)
    else NoMatch()

  implicit def toWrapperPC[M <: Match[Node], N <: Node](pattern: Pattern[Node])(
      implicit ctor: NodeCtor[M, N]
  ): WrapperPatternWithConstructor[NodeCtor[M, N]] =
    new WrapperPatternWithConstructor(pattern.as[Pattern[C#Input]], ctor)

}
