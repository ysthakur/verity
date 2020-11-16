package verity.parsing.parser

// import verity.parsing.lexer.{Token => _, _}
import verity.parsing.{Token, TokenType, TextRange}
import verity.parsing.ast._
import verity.parsing.ast.infile.expr._
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

  type P[N] = Pattern.Aux[N]

  val EOL = SEMICOLON

  val numLiteral: P[NumLiteral] = Pattern.fromOption(reader => reader.nextNumLiteral())
    |> { case Token(tr, text, _) => NumLiteral(text, tr) }

  val booleanLiteral = TRUE | FALSE |> { case token: Token => BoolLiteral(token) }

  val literal = numLiteral
    | booleanLiteral 
    | (KeywordToken.THIS |> { token => ThisRef(token.textRange)}) 
    | (KeywordToken.SUPER |> { token => SuperRef(token.textRange)})

  // val modifier: Pattern = (reader: Reader) => {
  //   if (reader.isEmpty) Matched.empty(reader, TextRange.empty(pos))
  //   else reader.head match {
  //     case Token(textRange, _, mod: ModifierTokenType) =>
  //       Matched(() => Modifier(mod.toModifier, textRange), reader/*.tail*/, textRange)
  //     case f => Failed(f, List("modifier"), pos)
  //   }
  // }

  val unaryPreOp = PLUSX2 | MINUSX2 | MINUS | EXCL_MARK | TILDE //|> opCtor
  val unaryPostOp = PLUSX2 | MINUSX2 //|> opCtor
  //TODO redo this
  val identifier: P[ValidIdNode] = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("identifier")
  ) |> {
    case Token(tr, name, _) => ValidIdNode(name, tr)
  }

  val unreservedId: P[ValidIdNode] = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("Unreserved identifier")
  ) |> {
    case Token(tr, name, _) => ValidIdNode(name, tr)
  }

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

  lazy val atom = parenExpr | literal | (unreservedId |> VarRef)

  val dotReference = identifier - ("." - identifier).* |> {
    case validId - nodes =>
      DotRef(
        validId :: nodes,
        if (nodes.isEmpty) validId.textRange
        else TextRange(validId.textRange.start, nodes.last.textRange.end)
      )
  }
  
  val pkgStmt = PACKAGE - dotReference - EOL |> {
    case (pkg: Token) - (dotRef: DotRef) - (eol: Token) =>
      //println(s"Is package $pkg - $dotRef - $eol")
      PackageStmt(dotRef, pkg.textRange to eol.textRange)
  }
  val importStatement =
    IMPORT - dotReference - (DOT - STAR).? - SEMICOLON |> {
      case (impt: Token) - (ref: DotRef) - star - eol => {
        //println(s"Is import $impt - $ref - $star")
        Import(ref, impt.textRange to eol.textRange, star != null)
      }
    }

  lazy val dotExpr = atom - (DOT - unreservedId).* |> {
    case (obj: Expr) - nodes => 
      nodes.foldLeft(obj: Expr){(p, n) => n match {
        case dot - (name: ValidIdNode) => DotChainedExpr(p, name): Expr
      }
    }
  }

  lazy val arrayAccess = dotExpr - (LSQUARE - expr - RSQUARE).* |> {
    case arr - nodes => nodes.foldLeft(arr: Expr) { (p, n) =>
      val l - index - r = n
      ArraySelect(p, index, p.textRange to r.textRange): Expr
    }
  }

  lazy val topExpr = 
    (((PLUS | MINUS | EXCL_MARK | TILDE).* - (PLUSX2 | MINUSX2).? - 
      atom - (DOT - unreservedId | LSQUARE - expr - RSQUARE).* -
      (PLUSX2 | MINUSX2).?).asInstanceOf[P[?]] |> {
    case preOps - preIncDec - (firstAtom: Expr) - (nodes: List[?]) - postIncDec => 
      nodes.foldLeft(firstAtom: Expr) { (p, n) =>
        n match {
          case dot - (name: ValidIdNode) => DotChainedExpr(p, name): Expr
          case l - (index: Expr) - (r: Token) => ArraySelect(p, index, p.textRange to r.textRange): Expr
        }
      }
  }).asInstanceOf[P[Expr]]

  lazy val unaryPost = unreservedId - (PLUSX2 | MINUSX2) |> {
    case (expr: Expr) - op => UnaryPostExpr(expr, opCtor(op))
  }
  lazy val preIncDec = (PLUSX2 | MINUSX2) - unreservedId |> {
    case op - (id: ValidIdNode) => UnaryPreExpr(opCtor(op), id)
  } 
  lazy val unaryPre = (PLUS | MINUS | EXCL_MARK | TILDE).* - topExpr |> {
    case ops - (expr: Expr) => ops.foldRight(expr: Expr) { (o, p) =>
      UnaryPreExpr(opCtor(o), p): Expr
    }
  }
  
  lazy val mulDivMod = binExpr(topExpr, STAR | FWDSLASH | MODULO)
  lazy val addSub = binExpr(mulDivMod, (PLUS | MINUS))
  lazy val bitExpr = binExpr(addSub, (LTX2 | GTX2 | GTX3))
  lazy val relationalExpr = binExpr(bitExpr, (LT | LTEQ | GT | GTEQ | INSTANCEOF))
  lazy val eqExpr = binExpr(relationalExpr, EQX2 | NOTEQ)
  lazy val bitAnd = binExpr(eqExpr, AND)
  lazy val bitXor = binExpr(bitAnd, CARET)
  lazy val bitOr = binExpr(bitXor, OR)
  lazy val logicAnd = binExpr(bitOr, ANDX2)
  lazy val logicOr = binExpr(logicAnd, ORX2)

  lazy val assignment = unreservedId - (PLUS | MINUS | STAR | FWDSLASH | MODULO).? - EQ - expr |> {
      case variable - op - eq - expr => ???
    }

  def binExpr[T <: Expr](
    prev: P[T], 
    operator: P[_ <: Token]
  )/*(ctor: (T, Token, T) => R = BinaryExpr(_, _, _))*/: P[Expr] = {
    // val foo: P[List[ConsNode[operator.Out, T]]] = (operator - prev).*
    // val fun: (List[ConsNode[operator.Out, T]]) => String = x => x.toString
    // foo |> fun
    prev - (operator - prev).* |> {
      case e1 - nodes =>
        nodes.foldLeft(e1: Expr){ (e, p) =>
          p match { case op - (e2: Expr) => BinaryExpr(e, op.text, e2) }
        }
      }
  }

  lazy val methodCall = {

  }

  lazy val valueArgList = LPAREN - expr - (COMMA - expr).* - RPAREN |> {
    case (lp: Token) - (arg1: Expr) - args - (rp: Token) => ArgList(
        List(arg1) ++ args.map{ case c - (arg: Expr) => arg },
        TextRange(lp.textRange.start, rp.textRange.end)
      )
  }
  
  /* lazy val sugaredApply = expr - valueArgList.* |> {
    case obj - nodes => 
      nodes.foldLeft(obj){ (p, n) =>
        ApplyCall(p, n.asInstanceOf[ArgList], obj.textRange to tr): Expr
      }
  } */

  lazy val parenExpr = "(" - expr - ")" |> { expr => ParenExpr(expr, expr.textRange) }

  val expr: P[Expr] = logicOr | assignment

  //TODO
  val typeRef = expr

  val varDeclFirstPart = typeRef - unreservedId

  val localVarDecl = varDeclFirstPart - EOL
  // "typeDecl" := "abcd"

  val root = expr |> {
    case expr => expr
  }

  extension (s: String)
    def -(next: Pattern): P[next.Out] =
      FunctionPattern{ reader =>
        val start = reader.offset
        reader.nextToken(s, TokenType.MISC, _.text == s) match {
          case None => Failed(Token.empty(start), List(s), start)
          case _ => next.tryMatch(reader)
        }
      }
}