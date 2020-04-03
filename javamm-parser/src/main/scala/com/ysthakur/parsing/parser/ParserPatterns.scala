package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast.infile.ValidIdNode
import com.ysthakur.parsing.ast.infile.expr._
import com.ysthakur.util._
import com.ysthakur.parsing.grammar._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer.{Token, TokenType, ValidIdentifierTokenType}
import com.ysthakur.parsing.parser._

import scala.language.postfixOps

object ParserPatterns {

  type P                   = Pattern[Node]
  type PC[T[_] <: Match[_], N <: Node] = PatternWithConstructor[T[Node], N]

  implicit val validIdCtor: SingleMatch[Node] => Node = 
    (m: SingleMatch[Node]) => new Node {}
  implicit val exprCtor: NodeCtor[CompositeMatch[Node], Expr] = 
    (m: CompositeMatch[Node]) => {
      null.asInstanceOf
    }

  val unaryPreOp: P         = PLUSX2 | MINUSX2
  val unaryPostOp: P        = PLUSX2 | MINUSX2
  lazy val unaryExpr: Any          = (expr - unaryPostOp) | (unaryPreOp - expr)
  private lazy val expr = (validId)
  val validId: PC[SingleMatch, Node] =
    FunctionPattern((input: Iterable[Node]) =>
      ifSingleTokenWithType(input) { _.isInstanceOf[ValidIdentifierTokenType] })
      .apply[SingleMatch[Node], Node](validIdCtor)
  val dotReference: ConsPattern[?, ?] = validId - ((DOT - validId) *)

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

  implicit def toWrapperPC[M <: Match[Node], N <: Node, I](pattern: Pattern[Node])(
      implicit ctor: M => N
  ): WrapperPatternWithConstructor[M, N] =
    new WrapperPatternWithConstructor(pattern, ctor)

}
