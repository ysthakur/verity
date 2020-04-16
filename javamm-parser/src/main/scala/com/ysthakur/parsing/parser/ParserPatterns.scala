package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast.infile.expr.{BinaryExpr, DotRef}
//import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer.TokenType._
import com.ysthakur.parsing.lexer._
import com.ysthakur.parsing.parser.Pattern.{*, -, ||}
import com.ysthakur.parsing.parser.{-, ||}

import scala.language.postfixOps

//noinspection ScalaUnnecessaryParentheses
private object ParserPatterns {

  type PAC[T <: Match[_], N <: Node] = PatternAndConstructor[T, N]
  type TTP = TokenTypePattern

  val opCtor = (op: Token[SymbolTokenType]) => Op(op)
  
  val EOL: Pattern = SEMICOLON
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE >> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 >> opCtor
  val identifier = FunctionPattern(
    (input: List[Node], offset: Int) => if (input.nonEmpty) input.head match {
      case token@Token(tt: ValidIdentifierTokenType) => 
        Matched(ValidIdNode(token.c), input.tail, offset + token.text.length)
      case h => Failed(h, List.empty)
    } else Failed("nothing", List.empty), List("valid identifier"))
  val dotReference = identifier - ((DOT - identifier)*) >> {
    case (validId) - (nodeList: NodeList[?]) =>
      DotRef(validId.asInstanceOf[ValidIdNode] :: (nodeList.asInstanceOf[NodeList[_]]).nodes.map(
        _.asInstanceOf[ConsNode[?, ValidIdNode]].n2
      ).toList)
  }
  val pkgStmt = PACKAGE - dotReference - EOL >> {
    case pkg - (dotRef: DotRef) - eol => PackageStmt(dotRef)
  }
  val importStatement = IMPORT - dotReference - ((DOT - STAR)?) >> {
    case impt - ref - star => Import(ref.c, star == null)
  }
  
  "unaryExpr" :=
    ("expr" - unaryPostOp) | (unaryPreOp - "expr") >> {
      case preExpr || postExpr => {
        preExpr match { 
          case op - expr => UnaryPreExpr(op.c, expr.c)
          case _ => postExpr match { case expr - op => UnaryPostExpr(expr.c, op.c) }
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
      | infix(ORX2) >> {
        case lexpr - op - rexpr => BinaryExpr(lexpr.as[Expr], op.as[Op], rexpr.as[Expr])
      }
  
  "dotSelect" := "expr" - DOT - identifier >> {
    case expr - dot - validId => DotChainedExpr(expr.c, validId.c)
  }
  "parenExpr" := LPAREN - "expr" - RPAREN >> { case lparen - expr - rparen => expr }
  "arrayAccess" := "expr" - LSQUARE - "expr" - RSQUARE >> {
    case (arr: Expr) - l - (index: Expr) - r => ArraySelect(arr, index)
  }
  "expr" :=
      identifier
      | "parenExpr"
      | "arrayAccess"
      | "dotSelect"
      | "unaryExpr"
      | "binaryExpr"

  val assignment = "expr" - ((PLUS | MINUS | STAR | FWDSLASH | MODULO)?) - EQ - "expr" - EOL >> {
    case variable - op - eq - expr - eol => ???
  }
  
  val root: Pattern = (pkgStmt?) - (importStatement*)
  
  /**
    * turn into an infix expression
    * @param pattern
    * @return
    */
  def infix(pattern: Pattern): Pattern = "expr" - pattern - "expr"
  
  def [T <: Node](n: Any).c: T = n.asInstanceOf[T]
  
  def [M <: Match[_], N <: Node](p: Pattern) >> (ctor: M => N): PatternAndConstructor[M, N] =
    PatternAndConstructor(p)
  
  implicit def unwrapOption[A](option: Option[A]): A = option.get
}
