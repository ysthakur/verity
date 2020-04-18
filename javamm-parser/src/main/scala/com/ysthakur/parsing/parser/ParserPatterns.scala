package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast.infile.expr.{BinaryExpr, DotRef}
import com.ysthakur.parsing.lexer._
import com.ysthakur.parsing.lexer.KeywordTokenType._
import com.ysthakur.parsing.lexer.RegexTokenType._
import com.ysthakur.parsing.lexer.SymbolTokenType._
import com.ysthakur.parsing.lexer.TokenType._
import com.ysthakur.parsing.parser.{-, ||}
import com.ysthakur.parsing.parser.Pattern.{*, -, ||}

import scala.language.postfixOps

//noinspection ScalaUnnecessaryParentheses
private object ParserPatterns {
  
  type TTP = TokenTypePattern

  val opCtor = (op: Node) => Op(op.asInstanceOf[Token[SymbolTokenType]])
  
  val EOL: Pattern = SEMICOLON
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE >> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 >> opCtor
  val identifier = FunctionPattern(
    (input: List[Tok], offset: Int) => if (input.nonEmpty) input.head match {
      case token@Token(tt: ValidIdentifierTokenType) => 
        Matched(() => ValidIdNode(token.c), input.tail, offset + token.text.length)
      case other => Failed(other, List("Valid identifier"), other.pos)
    } else Failed(EmptyToken, List("Valid identifier"), Position(0, 0, offset)))
  val dotReference = identifier - ((DOT - identifier)*) >> {
    case (validId: ValidIdNode) => DotRef(List(validId))
    case (validId: ValidIdNode) - (nodeList: NodeList[?]) =>
      DotRef(validId :: (nodeList.asInstanceOf[NodeList[_]]).nodes.map(
        _.asInstanceOf[ConsNode[?, ValidIdNode]].n2
      ).toList)
  }
  val pkgStmt = PACKAGE - dotReference - EOL >> {
    case pkg - (dotRef: DotRef) - eol => 
      println(s"Is package $pkg - $dotRef - $eol")
      PackageStmt(dotRef)
  }
  val importStatement = IMPORT - dotReference - ((DOT - STAR)?) - SEMICOLON >> {
    case impt - ref - star - semicolon => {
      println(s"Is import $impt - $ref - $star")
      Import(ref.c, star != null)
    }
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
  "parenExpr" := LPAREN - "expr" - RPAREN >> { case lparen - (expr: TextNode) - rparen => expr }
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
  
  def [N <: TextNode](p: Pattern) >> (ctor: TextNode => N): PatternAndConstructor[N] =
    PatternAndConstructor(p, ctor)
  
  implicit def unwrapOption[A](option: Option[A]): A = option.get
}