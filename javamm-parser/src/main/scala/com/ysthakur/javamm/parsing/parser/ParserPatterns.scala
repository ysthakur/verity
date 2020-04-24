package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.parsing.{EmptyToken, Position, TextRange, Token}
import com.ysthakur.javamm.parsing.ast.{ConsNode, NodeList, _}
import com.ysthakur.javamm.parsing.lexer.KeywordTokenType._
import com.ysthakur.javamm.parsing.lexer.RegexTokenType._
import com.ysthakur.javamm.parsing.lexer.SymbolTokenType._
import com.ysthakur.javamm.parsing.lexer.TokenType._
import com.ysthakur.javamm.parsing.lexer.{ModifierTokenType, ReservedWord, SymbolTokenType, Tok, ValidIdentifierTokenType}
import com.ysthakur.javamm.parsing.parser.Pattern.{*, -, ||}
import com.ysthakur.javamm.parsing.parser.{-, ||}

import scala.language.postfixOps

//noinspection ScalaUnnecessaryParentheses
private object ParserPatterns {

  type TTP = TokenTypePattern

  val opCtor = (op: Node) => Op(op.asInstanceOf[Token[SymbolTokenType]])

  val EOL: Pattern = SEMICOLON
  val numLiteral = NUM_LITERAL |>> {
    case token: Token[?] => NumLiteral(token.text)
  }
  val booleanLiteral = TRUE | FALSE |>> {
    case Token(TRUE) => TrueLiteral
    case Token(FALSE) => FalseLiteral
  }
  val literal = numLiteral | booleanLiteral | 
      (THIS |>> { case token: Tok => ThisRef(token.range)}) 
    | (SUPER |>> { case token: Tok => SuperRef(token.range)})
  val modifier: Pattern = (input: List[Tok], pos: Position, trace: Trace) => {
    if (input.isEmpty) Matched.empty(input, TextRange.empty(pos))
    else input.head match {
      case token@Token(mod: ModifierTokenType) => Matched(() => Modifier.get(mod), input.tail, token.range)
      case f => Failed(f, List("modifier"), pos)
    }
  }
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE |>> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 |>> opCtor
  val identifier = FunctionPattern((input: List[Tok], pos: Position) =>
    if (input.nonEmpty) input.head match {
      case token@Token(tt: ValidIdentifierTokenType) =>
        Matched(() => ValidIdNode(token.asInstanceOf), input.tail, token.range)
      case other => Failed(other, List("Identifier"), pos)
    }
    else Failed(EmptyToken, List("Identifier"), pos)
  )
  val unreservedId = FunctionPattern((input: List[Tok], pos: Position) => {
    if (input.nonEmpty) input.head match {
      case token @ Token(tt: ValidIdentifierTokenType) =>
        if (!token.isInstanceOf[ReservedWord])
          Matched(
              () =>
                ValidIdNode(
                    token.asInstanceOf[Token[ValidIdentifierTokenType]]
                ),
              input.tail,
              token.range
          )
        else Failed(token, List("Non-reserved identifier"), token.range.start)
      case other => Failed(other, List("Valid identifier"), pos)
    }
    Failed(EmptyToken, List("Valid identifier"), pos)
  })
  val dotReference = identifier - ((DOT - identifier) *) |>> {
    case (validId: ValidIdNode) => DotRef(List(validId))
    case (validId: ValidIdNode) - (nodeList: NodeList[?]) =>
      DotRef(
        validId :: (nodeList.asInstanceOf[NodeList[?]].nodes)
            .map(_.asInstanceOf[ConsNode[?, ValidIdNode]].n2.asInstanceOf)
            .toList)
  }
  val pkgStmt = PACKAGE - dotReference - EOL |>> {
    case pkg - (dotRef: DotRef) - eol =>
      //println(s"Is package $pkg - $dotRef - $eol")
      PackageStmt(dotRef)
  }
  val importStatement =
    IMPORT - dotReference - ((DOT - STAR) ?) - SEMICOLON |>> {
      case impt - (ref: DotRef) - star - semicolon => {
        //println(s"Is import $impt - $ref - $star")
        Import(ref, star != null)
      }
    }
  val firstLevelExpr = identifier | literal

  "unaryExpr" := ("expr" - unaryPostOp) | (unaryPreOp - "expr") |>> {
    case (op: Op) - (expr: Expr) => UnaryPreExpr(op, expr)
    case (expr: Expr) - (op: Op) => UnaryPostExpr(expr, op)
  }
  
  "binaryExpr" :=
    "expr" - (
      (STAR | FWDSLASH | MODULO)
      | (PLUS | MINUS)
      | (LTX2 | GTX2 | GTX3)
      | (LT | LTEQ | GT | GTEQ)
      | (EQX2 | NOTEQ)
      | AND
      | CARET
      | OR
      | ANDX2
      | ORX2) - "expr" |>> {
    case (lexpr: Expr) - op - (rexpr: Expr) =>
      println(s"GJGKJGJDFSG!!! - [$rexpr]");
      BinaryExpr(lexpr, opCtor(op), rexpr)
    case a - b - (c - d) => println(s"$c \n\t\t$d"); throw new Error("riyto8tq64")
    case x => println(s"QPPETQeRWJ#5 ${x.getClass()} $x"); x
  } Extends PatternRef("expr")

  "dotSelect" := "expr" - DOT - identifier |>> {
    case expr - dot - validId => DotChainedExpr(expr.asInstanceOf, validId.asInstanceOf)
  }
  "parenExpr" := LPAREN - "expr" - RPAREN |>> {
    case lparen - (expr: Node) - rparen => expr
  }
  "arrayAccess" := "expr" - LSQUARE - "expr" - RSQUARE |>> {
    case (arr: Expr) - l - (index: Expr) - r => ArraySelect(arr, index)
  }
  PatternClass.make(
      name = "expr",
      "binaryExpr",
      "dotSelect",
      "arrayAccess",
      "unaryExpr",
      firstLevelExpr,
      "parenExpr"
  )

  "typeRef" := ""

  val assignment =
    "expr" - ((PLUS | MINUS | STAR | FWDSLASH | MODULO) ?) - EQ - "expr" - EOL |>> {
      case variable - op - eq - expr - eol => ???
    }

  val varDeclFirstPart = "typeRef" - unreservedId |>> {
    case (typeRef: TypeRef) - (validId: ValidIdNode) =>
      ConsNode(typeRef, validId)
  }
  val localVarDecl = varDeclFirstPart - EOL
  "typeDecl" := "abcd"

  val root: Pattern = (pkgStmt?) - (importStatement*) - ("expr"?) |>> {
    case pkg - imports - typeDefs => typeDefs
  }

  def [N <: Node](p: Pattern) |>> (ctor: Node => N): PatternAndConstructor[N] =
    PatternAndConstructor(p, ctor)
}
