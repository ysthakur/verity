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

  val opCtor = (op: Node) => Op(op.text)

  val EOL: Pattern = SEMICOLON
  val numLiteral = FunctionPattern((toks, pos) => {/*println(s"num toks: $toks");*/TokenTypePattern(NUM_LITERAL).apply(toks, pos, null)}) |>> {
    case token: Token[?] => NumLiteral(token.text)
  }
  val booleanLiteral = TRUE | FALSE |>> {
    case Token(TRUE) => BoolLiteral.TrueLiteral
    case Token(FALSE) => BoolLiteral.FalseLiteral
  }
  val literal = numLiteral | booleanLiteral | 
      (THIS |>> { case token: Tok => ThisRef(token.range)}) 
    | (SUPER |>> { case token: Tok => SuperRef(token.range)})
  val modifier: Pattern = (input: List[Tok], pos: Position, trace: Trace) => {
    if (input.isEmpty) Matched.empty(input, TextRange.empty(pos))
    else input.head match {
      case token@Token(mod: ModifierTokenType) => Matched(() => mod.toModifier, input.tail, token.range)
      case f => Failed(f, List("modifier"), pos)
    }
  }
  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE |>> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 |>> opCtor
  val identifier = FunctionPattern((input: List[Tok], pos: Position) =>
    if (input.nonEmpty) input.head match {
      case token@Token(tt: ValidIdentifierTokenType) =>
        Matched(() => ValidIdNode(token.text), input.tail, token.range)
      case other => Failed(other, List("Identifier"), pos)
    }
    else Failed(EmptyToken, List("Identifier"), pos)
  )
  val unreservedId = FunctionPattern((input: List[Tok], pos: Position) => {
    if (input.nonEmpty) input.head match {
      case token @ Token(tt: ValidIdentifierTokenType) =>
        //println(s"Found token $token")
        if (!token.isInstanceOf[ReservedWord])
          Matched(() => ValidIdNode(token.text), input.tail, token.range)
        else 
          Failed(token, List("Non-reserved identifier"), token.range.start)
      case other => Failed(other, List("Valid identifier"), pos)
    }
    else Failed(EmptyToken, List("Valid identifier"), pos)
  })
  val dotReference = identifier - ((DOT - identifier) *) |>> {
    case (validId: ValidIdNode) => DotRef(List(validId))
    case (validId: ValidIdNode) - (nodeList: NodeList[?]) =>
      DotRef(
        validId :: (nodeList.asInstanceOf[NodeList[?]].nodes)
            .map(_.asInstanceOf[ConsNode[?, ValidIdNode]].n2.asInstanceOf[ValidIdNode])
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

  lazy val dotSelect = expr - DOT - identifier |>> {
    case expr - dot - validId => DotChainedExpr(expr.asInstanceOf, validId.asInstanceOf)
  }

  // lazy val dotExpr = expr - DOT - unreservedId /*|>> {
  //   case (expr: Expr) - dot - (id: ValidIdNode) => DotChainedExpr(expr, id)
  // }*/
  // lazy val indexExpr = expr - LSQUARE - expr - RSQUARE |>> {
  //   case obj - lsq - ind - rsq => ArraySelect(obj, ind)
  // }
   
  lazy val unaryPost = unreservedId - (PLUSX2 | MINUSX2) |>> {
    case (expr: Expr) - op => UnaryPostExpr(expr, opCtor(op))
  }
  lazy val unaryPre = (PLUSX2 | MINUSX2 | PLUS | MINUS | EXCL_MARK | TILDE) - unreservedId |>> {
    case op - (id: ValidIdNode) => UnaryPreExpr(opCtor(op), id)
  }

  lazy val actualTopExpr = unreservedId | parenExpr

  lazy val arrayAccess = actualTopExpr - LSQUARE - expr - RSQUARE |>> {
    case (arr: Expr) - l - (index: Expr) - r => ArraySelect(arr, index)
  }
  lazy val dotExpr = actualTopExpr - (DOT - unreservedId).* |>> {
    case (obj: Expr) - NodeList(nodes) => 
      nodes.foldLeft(obj){(p, n) => n match {
        case dot - (name: ValidIdNode) => DotChainedExpr(p, name)
      }
    }
  }

  lazy val topExpr = literal | unreservedId | parenExpr
   /*| dotExpr | indexExpr*/
  
  lazy val mulDivMod = binExpr(topExpr, STAR | FWDSLASH | MODULO)
    // ConsPattern(topExpr, RepeatPattern((STAR | FWDSLASH | MODULO) - topExpr, name="muldivmod"), "muldivcons1") |>> {
    //   case (e1: Expr) - NodeList(nodes) =>
    //     nodes.foldLeft(e1){(e, p) =>
    //     p match {
    //       case op - (e2: Expr) => BinaryExpr(e, opCtor(op), e2)
    //     }
    //   }
    // }
  
  lazy val addSub = binExpr(mulDivMod, (PLUS | MINUS))
    // ConsPattern(mulDivMod, RepeatPattern((PLUS | MINUS) - mulDivMod, name="addsub"), "addsubcons1") |>> {
    //   case (e1: Expr) - NodeList(nodes) =>
    //     nodes.foldLeft(e1){(e, p) =>
    //       p match {
    //         case op - (e2: Expr) => BinaryExpr(e, opCtor(op), e2)
    //       }
    //   }
    // }
  
  lazy val bitExpr = binExpr(addSub, (LTX2 | GTX2 | GTX3))
  lazy val relationalExpr = binExpr(bitExpr, (LT | LTEQ | GT | GTEQ | IS))
  lazy val eqExpr = binExpr(relationalExpr, (EQX2 - NOTEQ))
  lazy val bitAnd = binExpr(eqExpr, AND)
  lazy val bitXor = binExpr(bitAnd, CARET)
  lazy val bitOr = binExpr(bitXor, OR)
  lazy val logicAnd = binExpr(bitOr, ANDX2)
  lazy val logicOr = binExpr(logicAnd, ORX2)
  lazy val assignment = unreservedId - (PLUS | MINUS | STAR | FWDSLASH | MODULO).? - EQ - expr |>> {
      case variable - NodeList(Seq(op)) - eq - expr - eol => ???
    }

  def binExpr(prev: Pattern, operator: Pattern) =
    prev - (operator - prev).* |>> {
      case (e1: Expr) - NodeList(nodes) =>
        nodes.foldLeft(e1){(e, p) =>
          p match {
            case op - (e2: Expr) => BinaryExpr(e, opCtor(op), e2)
          }
      }
    }

  lazy val methodCall = {

  }

  lazy val valueArgList = LPAREN - expr - (COMMA - expr).* - RPAREN |>> {
    case (lp: Tok) - (arg1: Expr) - NodeList(args) - (rp: Tok) => ArgList(
      List(arg1) ++ args.map{ case c - (arg: Expr) => arg },
      TextRange(lp.range.start, rp.range.end))
  }
  
  // import com.ysthakur.verity.parsing.ast.infile.expr._
  
  lazy val sugaredApply = expr - valueArgList.* |>> {
    case (obj: Expr) - NodeList(nodes) => nodes.foldLeft(obj){(p, n) => ApplyCall(p, n.asInstanceOf[ArgList])}
  }

  lazy val expr: Pattern = unaryPost | unaryPre | logicOr | assignment

  lazy val exprList = expr - ((COMMA - expr)*)

  lazy val parenExpr = ConsPattern(ConsPattern(LPAREN, expr, "pareninner"), RPAREN, "parenouter") |>> {
    case (lparen: Tok) - (expr: Expr) - (rparen: Tok) => 
      ParenExpr(expr, lparen.range.start.to(rparen.range.end))
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