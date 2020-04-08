package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast.infile.ValidIdNode
import com.ysthakur.parsing.ast.infile.expr._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer._
import com.ysthakur.parsing.parser.PatternUtils._
import com.ysthakur.util._

import scala.language.postfixOps

private object ParserPatterns {

  type PAC[T <: Match[_], N <: Node] = PatternAndConstructor[T, N]

  implicit val validIdCtor: SingleMatch[Token] => ValidIdNode =
    (m) => new ValidIdNode(m.matchedPiece.text, m.matchedPiece.startOffset, m.matchedPiece.endOffset)
  implicit val exprCtor: CompositeMatch[Node] => Expr =
    (m: CompositeMatch[Node]) => {
      null.asInstanceOf
    }

  val a: TokenTypePattern = PLUSX2
  val b: TokenTypePattern = MINUSX2
  val c: Null = null
  type ev = a.type =:= TokenTypePattern
  type ev2 = b.type =:= TokenTypePattern
  val x: OrPattern[TokenTypePattern, TokenTypePattern] = (a | b)

  val unaryPreOp: OrPattern[_, _] = 
    (PLUSX2 | MINUSX2) | MINUS | EXCL_MARK
  val unaryPostOp = PLUSX2 | MINUSX2
  lazy val unaryExpr = (expr - unaryPostOp) | (unaryPreOp - expr)
  lazy val expr = (validId)
  val validId =
    FunctionPattern[Node, SingleMatch[Token], ValidIdNode](
      (input: Iterable[Node], offset: Int) => 
        ifSingleTokenWithType(input) { _.isInstanceOf[ValidIdentifierTokenType] }, validIdCtor)
  val dotReference: ConsPattern[?, ?] = validId - ((DOT - validId) *)

  def ifSingleTokenWithType(
      input: Iterable[Node]
  )(f: TokenType => Boolean): MatchResult = {
    ifSingleToken(input) {
      case token: Token =>
        if (f(token.tokenType))
          FullMatch(SingleMatch(token, 0), couldMatchMore = false)
        else NoMatch
      case _ => NoMatch
    }
  }

  def ifSingleToken(
      input: Iterable[Node]
  )(f: Node => MatchResult): MatchResult =
    if (input.size == 1) f(input.head)
    else NoMatch

//   implicit def toWrapperPC[M <: Match[Node], N <: Node, I](pattern: Pattern[Node])(
//       implicit ctor: M => N
//   ): PatternAndConstructor[M, N] = PatternAndConstructor(pattern, ctor)

}
