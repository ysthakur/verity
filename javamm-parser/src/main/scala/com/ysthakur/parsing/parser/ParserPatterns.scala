package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.infile.ValidIdNode
import com.ysthakur.parsing.ast.infile.expr._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer._
import com.ysthakur.parsing.parser.Pattern.{*, -, ||}
import com.ysthakur.parsing.parser.{-, ||}
import com.ysthakur.util._

import scala.language.postfixOps

private object ParserPatterns {

  type PAC[T <: Match[_], N <: Node] = PatternAndConstructor[T, N]
  type TTP = TokenTypePattern

  val opCtor = (op: Token[SymbolTokenType]) => Op(op)
  
  val root = null
  
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK >> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 >> opCtor
  val identifier = FunctionPattern(
    (input: List[Node], _) => input.head match {
      case token@Token(tt: ValidIdentifierTokenType, _, _) => 
        Matched(ValidIdNode(token), input.tail, token.endOffset)
    })
  "unaryExpr" :=
    ("expr" - unaryPostOp) | (unaryPreOp - "expr") >> {
      case preExpr || postExpr => {
        preExpr match { 
          case op - expr => UnaryPreExpr(op, expr)
          case _ => postExpr match { case expr - op => UnaryPostExpr(expr, op) }
        }
      }
    }
  "dotReference" := "validId" - ((DOT - "validId")*) >> {
    case validId - (nodeList: NodeList[?]) =>
      DotRef(validId :: nodeList.nodes.map(
        _.asInstanceOf[ConsNode[?, ValidIdNode]].n2
      ).toList)
  }
  "dotChainedExpr" := "expr" - DOT - "validId" >> {
    case expr - dot - validId => DotChainedExpr(expr, validId)
  }
  "expr" :=
    PatternClass[Expr](
      identifier,
      "unaryExpr",
      "dotReference",
      "dotChainedExpr")

  implicit def c[T <: Node](n: Node): T = n.asInstanceOf[T]
  
  def [M <: Match[_], N <: Node](p: Pattern) >> (ctor: M => N): PatternAndConstructor[M, N] =
    PatternAndConstructor(p, ctor)
  
  implicit def unwrapOption[A](option: Option[A]): A = option.get
  
  /*implicit val validIdCtor: SingleMatch[Token[ValidIdentifierTokenType]] => ValidIdNode =
    (m) =>
      new ValidIdNode(m.matchedPiece
          /*m.matchedPiece.text.toString,
          m.matchedPiece.startOffset,
          m.matchedPiece.endOffset*/
      )
  implicit val exprCtor: CompositeMatch[Node] => Expr =
    (m: CompositeMatch[Node]) => {
      null.asInstanceOf
    }
  
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK
  val unaryPostOp = PLUSX2 | MINUSX2
  lazy val unaryExpr = (expr - unaryPostOp) | (unaryPreOp - expr)
  lazy val varRefExpr = validId
  lazy val expr = 
    PatternClass[Expr](
      unaryExpr, 
      varRefExpr,
      dotReference)
  val validId =
    FunctionPattern[Node, SingleMatch[Token[ValidIdentifierTokenType]], ValidIdNode](
        (input: Iterable[Node], offset: Int) =>
          ifSingleTokenWithType(input) {
            _.isInstanceOf[ValidIdentifierTokenType]
          },
        validIdCtor
    )
  lazy val dotReference = validId - ((DOT - validId) *)
  lazy val dotChainedExpr = expr*/
/*
  def ifSingleTokenWithType(
      input: Iterable[Node]
  )(f: TokenType => Boolean): ParseResult = {
    ifSingleToken(input) {
      case token: Token[?] =>
        if (f(token.tokenType))
          FullMatch(SingleMatch(token, 0), couldMatchMore = false)
        else NoMatch
      case _ => NoMatch
    }
  }

  def ifSingleToken(
      input: Iterable[Node]
  )(f: Node => ParseResult): ParseResult =
    if (input.size == 1) f(input.head)
    else NoMatch*/

//   implicit def toWrapperPC[M <: Match[Node], N <: Node, I](pattern: Pattern[Node])(
//       implicit ctor: M => N
//   ): PatternAndConstructor[M, N] = PatternAndConstructor(pattern, ctor)

}
