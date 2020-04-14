package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer.TokenType._
import com.ysthakur.parsing.lexer._
import com.ysthakur.parsing.parser.Pattern.{*, -, ||}
import com.ysthakur.parsing.parser.{-, ||}

import scala.language.postfixOps

private object ParserPatterns {

  type PAC[T <: Match[_], N <: Node] = PatternAndConstructor[T, N]
  type TTP = TokenTypePattern

  val opCtor = (op: Token[SymbolTokenType]) => Op(op)
  
  val root: Pattern = "importStatement"
  val EOL: Pattern = SEMICOLON
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE >> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 >> opCtor
  val identifier = FunctionPattern(
    (input: List[Node], offset: Int) => input.head match {
      case token@Token(tt: ValidIdentifierTokenType) => 
        Matched(ValidIdNode(token), input.tail, offset + token.text.length)
    })
  val dotReference = identifier - ((DOT - identifier)*) >> {
    case validId - (nodeList: NodeList[?]) =>
      DotRef(validId :: nodeList.nodes.map(
        _.asInstanceOf[ConsNode[?, ValidIdNode]].n2
      ).toList)
  }
  val importStatement = IMPORT - dotReference - ((DOT - STAR)?) >> {
    case impt - ref - star => ???
  }
  
  "unaryExpr" :=
    ("expr" - unaryPostOp) | (unaryPreOp - "expr") >> {
      case preExpr || postExpr => {
        preExpr match { 
          case op - expr => UnaryPreExpr(op, expr)
          case _ => postExpr match { case expr - op => UnaryPostExpr(expr, op) }
        }
      }
    }
  
  "binaryExpr" := 
      infix(STAR | FWDSLASH | MODULO)
      | infix(PLUS | MINUS)
      | infix(LTX2 | GTX2 | GTX3)
      | infix(LT | LTEQ | GT | GTEQ)
      | infix(EQX2 | NOTEQ)
      | infix(AND)
      | infix(CARET)
      | infix(OR)
      | infix(ANDX2)
      | infix(ORX2)
  
  "dotSelect" := "expr" - DOT - identifier >> {
    case expr - dot - validId => DotChainedExpr(expr, validId)
  }
  "parenExpr" := LPAREN - "expr" - RPAREN >> { case lparen - expr - rparen => expr }
  "arrayAccess" := "expr" - LSQUARE - "expr" - RSQUARE
  "expr" :=
    PatternClass[Expr](
      identifier,
      "parenExpr",
      "arrayAccess",
      "dotSelect",
      "unaryExpr",
      "binaryExpr")

  "assignment" := "expr" - ((PLUS | MINUS | STAR | FWDSLASH | MODULO)?) - EQ - "expr" - EOL >> {
    case variable - op - eq - expr - eol => ???
  }
  
  /**
    * turn into an infix expression
    * @param pattern
    * @return
    */
  def infix(pattern: Pattern): Pattern = "expr" - pattern - "expr"
  
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
