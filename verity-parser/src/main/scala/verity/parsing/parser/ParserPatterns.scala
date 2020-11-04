package verity.parsing.parser

// import verity.parsing.lexer.{Token => _, _}
import verity.parsing.{Token, TokenType, TextRange}
import verity.parsing.ast._
import verity.parsing.ast.infile.expr._
// import verity.parsing.parser.Pattern._
import SymbolToken._
import ModifierToken._
import KeywordToken._

/* import verity.parsing.lexer.SymbolTokenType._
import verity.parsing.lexer.RegexTokenType._
import verity.parsing.lexer.KeywordTokenType._
import verity.parsing.lexer.ReservedWord._
import verity.parsing.lexer.TokenType._ */

private object ParserPatterns {

  val opCtor = (op: Node) => Op(op.text, op.textRange)

  val EOL: Pattern = SEMICOLON

  val numLiteral = Pattern.fromOption((reader: Reader) => reader.nextNumLiteral())

  val booleanLiteral = TRUE | FALSE |>> { case token: Token => BoolLiteral(token) }

  val literal = numLiteral | booleanLiteral | 
      (KeywordToken.THIS |>> { case token: Token => ThisRef(token.textRange)}) 
    | (KeywordToken.SUPER |>> { case token: Token => SuperRef(token.textRange)})

  // val modifier: Pattern = (reader: Reader) => {
  //   if (reader.isEmpty) Matched.empty(reader, TextRange.empty(pos))
  //   else reader.head match {
  //     case Token(textRange, _, mod: ModifierTokenType) =>
  //       Matched(() => Modifier(mod.toModifier, textRange), reader/*.tail*/, textRange)
  //     case f => Failed(f, List("modifier"), pos)
  //   }
  // }

  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE |>> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 |>> opCtor
  //TODO redo this
  val identifier = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("identifier")
  )

  val unreservedId = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("Unreserved identifier")
  )

  /*val unreservedId = FunctionPattern((reader: Reader) => {
    val start = reader.offset
    if (reader.nonEmpty) reader.head match {
      case token @ Token(textRange, text, tt: ValidIdentifierTokenType) =>
        if (!Token.hardKeywords.contains(text))
          Matched(() => ValidIdNode(text, textRange), reader, textRange)
        else 
          Failed(token, List("Non-reserved identifier"), textRange.start)
      case other => Failed(other, List("Valid identifier"), start)
    }
    else Failed(null, List("Valid identifier"), start)
  })*/
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
    IMPORT - dotReference - (DOT - STAR).? - SEMICOLON |>> {
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
      val l - (index: Expr) - (r: Token) = n
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
        case l - (index: Expr) - (r: Token) => ArraySelect(p, index, p.textRange to r.textRange): Expr
        case dot - (name: ValidIdNode) => DotChainedExpr(p, name): Expr
      }
    }
    case x => println(x);throw new Error("foo")
  }

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
  lazy val relationalExpr = binExpr(bitExpr, (LT | LTEQ | GT | GTEQ | INSTANCEOF))
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
    case (lp: Token) - (arg1: Expr) - NodeList(args, tr) - (rp: Token) => ArgList(
        List(arg1) ++ args.map{ case c - (arg: Expr) => arg },
        TextRange(lp.textRange.start, rp.textRange.end)
      )
  }
  
  // import verity.parsing.ast.infile.expr._
  
  lazy val sugaredApply = expr - valueArgList.* |>> {
    case (obj: Expr) - NodeList(nodes, tr) => 
      nodes.foldLeft(obj: Expr){(p, n) => ApplyCall(p, n.asInstanceOf[ArgList], obj.textRange to tr): Expr}
  }

  lazy val expr: Pattern = logicOr | assignment

  lazy val exprList = expr - (COMMA - expr).*

  lazy val parenExpr = ConsPattern(ConsPattern(LPAREN, expr, "pareninner"), RPAREN, "parenouter") |>> {
    case (lparen: Token) - (expr: Expr) - (rparen: Token) => 
      ParenExpr(expr, TextRange(lparen.textRange.start, rparen.textRange.end))
  }

  //TODO
  val typeRef = expr

  val varDeclFirstPart = typeRef - unreservedId |>> {
    case (typeRef: verity.parsing.ast.infile.TypeRef) - (validId: ValidIdNode) =>
      ConsNode(typeRef, validId)
  }
  val localVarDecl = varDeclFirstPart - EOL
  // "typeDecl" := "abcd"

  val root: Pattern = expr |>> {
    case expr => expr
  }

  def [N <: Node](p: Pattern) |>> (ctor: Node => N): PatternAndConstructor[N] =
    PatternAndConstructor(p, ctor)
}