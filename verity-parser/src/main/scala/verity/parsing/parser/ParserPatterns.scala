package verity.parsing.parser

import verity.parsing.{TextRange, Token, TokenType}
import verity.parsing.ast._
import verity.parsing.ast.infile.DotRef
import verity.parsing.ast.infile.expr._
import verity.parsing.parser.SymbolToken._
import verity.parsing.parser.KeywordToken._

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
  val identifier: P[ValidId] = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("identifier")
  ) |> {
    case Token(tr, name, _) => ValidId(name, tr)
  }

  val unreservedId: P[ValidId] = Pattern.fromOption(
    reader => reader.nextAlphaNum(),
    List("Unreserved identifier")
  ) |> {
    case Token(tr, name, _) => ValidId(name, tr)
  }

  /*val unreservedId = FunctionPattern((reader: Reader) => {
    val start = reader.offset
    if (reader.nonEmpty) reader.head match {
      case token @ Token(textRange, text, tt: ValidIdentifierTokenType) =>
        if (!Token.hardKeywords.contains(text))
          Matched(() => ValidId(text, textRange), reader, textRange)
        else 
          Failed(token, List("Non-reserved identifier"), textRange.start)
      case other => Failed(other, List("Valid identifier"), start)
    }
    else Failed(null, List("Valid identifier"), start)
  })*/

  val parenExpr = "(" - ByNameP(expr) - ")" |> { expr => ParenExpr(expr, expr.textRange) }
  val atom = parenExpr | literal | (unreservedId |> VarRef)

  
//
//  lazy val dotExpr = atom - ("." - unreservedId).* |> {
//    case obj - nodes => nodes.foldLeft(obj: Expr)(DotChainedExpr)
//  }
//
//  lazy val arrayAccess = dotExpr - (LSQUARE - expr - RSQUARE).* |> {
//    case arr - nodes => nodes.foldLeft(arr: Expr) { (p, n) =>
//      val l - index - r = n
//      ArraySelect(p, index, p.textRange to r.textRange): Expr
//    }
//  }

  val topExpr: P[Expr] = 
    ((PLUS | MINUS | EXCL_MARK | TILDE).* - (PLUSX2 | MINUSX2).? - 
      ByNameP(atom) - (DOT - identifier | LSQUARE - ByNameP(expr) - RSQUARE).* -
      (PLUSX2 | MINUSX2).?) |> {
    case preOps - preIncDec - firstAtom - nodes - postIncDec => 
      nodes.foldLeft(firstAtom: Expr) { (p, n) =>
        n match {
          case l - (index: Expr) - (r: Token) => ArraySelect(p, index, p.textRange to r.textRange): Expr
          case dot - (name: ValidId) => DotChainedExpr(p, name): Expr
        }
      }
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
  
  val mulDivMod = binExpr(topExpr, STAR | FWDSLASH | MODULO)
  val addSub = binExpr(mulDivMod, (PLUS | MINUS))
  val bitExpr = binExpr(addSub, (LTX2 | GTX2 | GTX3))
  val relationalExpr = binExpr(bitExpr, (LT | LTEQ | GT | GTEQ | INSTANCEOF))
  val eqExpr = binExpr(relationalExpr, EQX2 | NOTEQ)
  val bitAnd = binExpr(eqExpr, AND)
  val bitXor = binExpr(bitAnd, CARET)
  val bitOr = binExpr(bitXor, OR)
  val logicAnd = binExpr(bitOr, ANDX2)
  val logicOr = binExpr(logicAnd, ORX2)

  //TODO don't use identifier, also allow indexing and field access
  lazy val assignment = (identifier - (PLUS | MINUS | STAR | FWDSLASH | MODULO).? - "=").*\ - logicOr |> {
      case assignments - rhs => assignments.foldLeft(rhs: Expr) { (r, l) =>
        l match {
          case lhs - op => new AssignmentExpr(lhs, r, op)
        }
      }
    }

  def binExpr[T <: Expr](
    prev: => P[T], 
    operator: P[_ <: Token]
  ): P[Expr] = {
    val byNamePrev = ByNameP(prev)
    byNamePrev - (operator - byNamePrev).* |> {
      case e1 - nodes =>
        nodes.foldLeft(e1: Expr){ (e, p) =>
          p match { case op - (e2: Expr) => BinaryExpr(e, op.text, e2) }
        }
      }
  }

  val expr: P[Expr] = logicOr | assignment

  //TODO
  lazy val wildcard = QUESTION - (EXTENDS - typeRef).? - (SUPER - typeRef).? |> {
    case q - ext - sup => Wildcard(if (ext != null) ext.n2 else null, if (sup != null) sup.n2 else null)
  }
  lazy val typeArg = typeRef | wildcard
  lazy val typeArgs = typeArg - ("," - typeArg).* |> { case firstType - nextTypes => firstType :: nextTypes }
  val typeRef: P[TypeRef] = identifier - ("<" - ByNameP(typeArgs) - ">").? |> { 
    case name - args => TypeRef(name, args ?: Nil)
  }

  val varDeclFirstPart = typeRef - unreservedId

  val localVarDecl = varDeclFirstPart - EOL

  val dotReference = identifier - ("." - identifier).* |> {
    case validId - nodes =>
      DotRef(
        validId :: nodes,
        if (nodes.isEmpty) validId.textRange
        else TextRange(validId.textRange.start, nodes.last.textRange.end)
      )
  }
  val pkgStmt = PACKAGE - dotReference - EOL |> {
    case pkg - dotRef - eol => PackageStmt(dotRef, pkg.textRange to eol.textRange)
  }
  val importStatement = IMPORT - dotReference - ("." - STAR).? - SEMICOLON |> {
      case impt - ref - star - eol => Import(ref, impt.textRange to eol.textRange, star != null)
    }
  
  val root = expr |> {
    case expr => expr
  }

  extension (s: String)
    def -(next: Pattern): P[next.Out] = reader => {
      val start = reader.offset
      reader.nextToken(s, TokenType.MISC, _.text == s) match {
        case None => Failed(Token.empty(start), List(s), start)
        case _ => next.tryMatch(reader)
      }
    }

  extension [A <: AnyRef](anyref: A|Null) inline def ?:(alt: => A): A = if anyref != null then anyref.nn else alt
}