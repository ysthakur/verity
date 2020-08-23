package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.parsing.lexer._
import com.ysthakur.verity.parsing.{Position, TextRange}
import com.ysthakur.verity.parsing.ast._
import com.ysthakur.verity.parsing.ast.infile.expr._
import com.ysthakur.verity.parsing.lexer.SymbolTokenType._
import com.ysthakur.verity.parsing.lexer.RegexTokenType._
import com.ysthakur.verity.parsing.lexer.KeywordTokenType._
import com.ysthakur.verity.parsing.lexer.ReservedWord._
import com.ysthakur.verity.parsing.lexer.TokenType._
import com.ysthakur.verity.parsing.parser.Pattern._

import scala.language.postfixOps

//noinspection ScalaUnnecessaryParentheses
private object ParserPatterns {

  //type TTP = TokenTypePattern

  val opCtor = (op: Node) => Op(op.text, op.textRange)

  val EOL: Pattern = SEMICOLON
  val numLiteral = NUM_LITERAL |>> {
    case token: Token[?] => NumLiteral(token.text, token.textRange)
  }
  val booleanLiteral = TRUE | FALSE |>> {
    case t@Token(TRUE) => BoolLiteral.TrueLiteral(t.textRange)
    case t@Token(FALSE) => BoolLiteral.FalseLiteral(t.textRange)
  }
  val literal = numLiteral | booleanLiteral | 
      (THIS |>> { case token: Tok => ThisRef(token.textRange)}) 
    | (SUPER |>> { case token: Tok => SuperRef(token.textRange)})

  val modifier: Pattern = (input: List[Tok], pos: Position, trace: Trace) => {
    if (input.isEmpty) Matched.empty(input, TextRange.empty(pos))
    else input.head match {
      case token@Token(mod: ModifierTokenType) => Matched(() => Modifier(mod.toModifier, token.textRange), input.tail, token.textRange)
      case f => Failed(f, List("modifier"), pos)
    }
  }

  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE |>> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 |>> opCtor
  val identifier = FunctionPattern((input: List[Tok], pos: Position) =>
    if (input.nonEmpty) input.head match {
      case token@Token(tt: ValidIdentifierTokenType) =>
        Matched(() => ValidIdNode(token.text, token.textRange), input.tail, token.textRange)
      case other => Failed(other, List("Identifier"), pos)
    }
    else Failed(null, List("Identifier"), pos)
  )
  val unreservedId = FunctionPattern((input: List[Tok], pos: Position) => {
    if (input.nonEmpty) input.head match {
      case token @ Token(tt: ValidIdentifierTokenType) =>
        if (!token.isInstanceOf[ReservedWord])
          Matched(() => ValidIdNode(token.text, token.textRange), input.tail, token.textRange)
        else 
          Failed(token, List("Non-reserved identifier"), token.textRange.start)
      case other => Failed(other, List("Valid identifier"), pos)
    }
    else Failed(null, List("Valid identifier"), pos)
  })
  val dotReference = identifier - (DOT - identifier).* |>> {
    case (validId: ValidIdNode) - NodeList(nodes, tr) =>
      DotRef(
        validId :: nodes
            .map(_.asInstanceOf[ConsNode[?, ValidIdNode]].n2.asInstanceOf[ValidIdNode])
            .toList, validId.textRange to tr)
  }
  val pkgStmt = PACKAGE - dotReference - EOL |>> {
    case pkg - (dotRef: DotRef) - eol =>
      //println(s"Is package $pkg - $dotRef - $eol")
      PackageStmt(dotRef, pkg.textRange to eol.textRange)
  }
  val importStatement =
    IMPORT - dotReference - ((DOT - STAR) ?) - SEMICOLON |>> {
      case impt - (ref: DotRef) - star - eol => {
        //println(s"Is import $impt - $ref - $star")
        Import(ref, impt.textRange to eol.textRange, star != null)
      }
    }

  lazy val atom = literal | unreservedId | parenExpr

  lazy val dotExpr = atom - (DOT - unreservedId).* |>> {
    case (obj: Expr) - NodeList(nodes, tr) => 
      nodes.foldLeft(obj: Expr){(p, n) => n match {
        case dot - (name: ValidIdNode) => DotChainedExpr(p, name): Expr
      }
    }
  }

  lazy val arrayAccess = dotExpr - (LSQUARE - expr - RSQUARE).* |>> {
    case (arr: Expr) - NodeList(nodes, tr) => nodes.foldLeft(arr: Expr) { (p, n) =>
      val l - (index: Expr) - (r: Tok) = n
      ArraySelect(p, index, p.textRange to r.textRange): Expr
    }
  }

  lazy val topExpr = 
    (PLUS | MINUS | EXCL_MARK | TILDE).* - (PLUSX2 | MINUSX2).* - 
      atom - (DOT - unreservedId | LSQUARE - expr - RSQUARE).* -
      (PLUSX2 | MINUSX2).? |>> {
    case NodeList(preOps, potr) - NodeList(preIncDec, pridtr) - 
      (obj: Expr) - NodeList(nodes, ntr) - NodeList(postIncDec, poidtr) => nodes.foldLeft(obj: Expr) { (p, n) =>
      n match {
        case l - (index: Expr) - (r: Tok) => ArraySelect(p, index, p.textRange to r.textRange): Expr
        case dot - (name: ValidIdNode) => DotChainedExpr(p, name): Expr
      }
    }
    case x => println(x);throw new Error("foo")
  }

  // ConsNode(
  //   ConsNode(
  //     ConsNode(
  //       ConsNode(
  //         NodeList(List()),
  //         NodeList(List())
  //       ),
  //       NumLiteral(9,TextRange(Position(10,0,358),Position(10,1,359)))
  //     ),
  //     NodeList(List())
  //   ),
  //   NodeList(List()))

  lazy val unaryPost = unreservedId - (PLUSX2 | MINUSX2) |>> {
    case (expr: Expr) - op => UnaryPostExpr(expr, opCtor(op))
  }
  lazy val preIncDec = (PLUSX2 | MINUSX2) - unreservedId |>> {
    case op - (id: ValidIdNode) => UnaryPreExpr(opCtor(op), id)
  } 
  lazy val unaryPre = (PLUS | MINUS | EXCL_MARK | TILDE).* - topExpr |>> {
    case NodeList(ops, tr) - (expr: Expr) => ops.foldRight(expr: Expr) { (o, p) =>
      UnaryPreExpr(opCtor(o), p): Expr
    }
  }
  
  lazy val mulDivMod = binExpr(topExpr, STAR | FWDSLASH | MODULO)
  lazy val addSub = binExpr(mulDivMod, (PLUS | MINUS))
  lazy val bitExpr = binExpr(addSub, (LTX2 | GTX2 | GTX3))
  lazy val relationalExpr = binExpr(bitExpr, (LT | LTEQ | GT | GTEQ | IS))
  lazy val eqExpr = binExpr(relationalExpr, (EQX2 - NOTEQ))
  lazy val bitAnd = binExpr(eqExpr, AND)
  lazy val bitXor = binExpr(bitAnd, CARET)
  lazy val bitOr = binExpr(bitXor, OR)
  lazy val logicAnd = binExpr(bitOr, ANDX2)
  lazy val logicOr = binExpr(logicAnd, ORX2)

  lazy val assignment = unreservedId - (PLUS | MINUS | STAR | FWDSLASH | MODULO).? - EQ - expr |>> {
      case variable - NodeList(Seq(op), tr) - eq - expr - eol => ???
    }

  def binExpr(prev: Pattern, operator: Pattern) =
    prev - (operator - prev).* |>> {
      case (e1: Expr) - NodeList(nodes, tr) =>
        nodes.foldLeft(e1: Expr){(e, p) =>
          p match {
            case op - (e2: Expr) => BinaryExpr(e, opCtor(op), e2): Expr
          }
      }
      case x => println("foo\n" + operator + "\n" + x);x
    }

  lazy val methodCall = {

  }

  lazy val valueArgList = LPAREN - expr - (COMMA - expr).* - RPAREN |>> {
    case (lp: Tok) - (arg1: Expr) - NodeList(args, tr) - (rp: Tok) => ArgList(
        List(arg1) ++ args.map{ case c - (arg: Expr) => arg },
        TextRange(lp.textRange.start, rp.textRange.end)
      )
  }
  
  // import com.ysthakur.verity.parsing.ast.infile.expr._
  
  lazy val sugaredApply = expr - valueArgList.* |>> {
    case (obj: Expr) - NodeList(nodes, tr) => 
      nodes.foldLeft(obj: Expr){(p, n) => ApplyCall(p, n.asInstanceOf[ArgList], obj.textRange to tr): Expr}
  }

  lazy val expr: Pattern = logicOr | assignment

  lazy val exprList = expr - ((COMMA - expr)*)

  lazy val parenExpr = ConsPattern(ConsPattern(LPAREN, expr, "pareninner"), RPAREN, "parenouter") |>> {
    case (lparen: Tok) - (expr: Expr) - (rparen: Tok) => 
      ParenExpr(expr, lparen.textRange.start.to(rparen.textRange.end))
  }

  val varDeclFirstPart = "typeRef" - unreservedId |>> {
    case (typeRef: com.ysthakur.verity.parsing.ast.infile.TypeRef) - (validId: ValidIdNode) =>
      ConsNode(typeRef, validId)
  }
  val localVarDecl = varDeclFirstPart - EOL
  "typeDecl" := "abcd"

  val root: Pattern = expr |>> {
    case expr => expr
  }

  def [N <: Node](p: Pattern) |>> (ctor: Node => N): PatternAndConstructor[N] =
    PatternAndConstructor(p, ctor)
}