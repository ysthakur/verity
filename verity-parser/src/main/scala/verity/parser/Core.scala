package verity.parser

import verity.ast._

import Parser.ps2tr

import cats.parse.Rfc5234.{wsp, alpha, digit}

private class Core(implicit offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]) {

  /** Negative lookahead to ensure that an identifier has ended
    */
  private def nid: Parser[Unit] = Parser.end | Parser.anyChar.filter(_.isUnicodeIdentifierPart).peek

  def identifier(id: String): Parser[Unit] = Parser.string(id) ~ nid

  def identifier[_: Parser]: Parser[String] =
    Parser(CharPred(_.isUnicodeIdentifierStart).! ~~ CharsWhile(_.isUnicodeIdentifierPart, 0).!)
      .map { case (first, rest) => first + rest }
      .filter(s => !hardKeywords(s))

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple doesn't have to be
    * turned into a Text object later
    */
  def identifierText[_: Parser]: Parser[Text] =
    Parser(identifier ~ Index).map { case (id, end) => Text(id, ps2tr(end - id.length, end)) }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  def identifierWithTextRange[_: Parser]: Parser[(String, TextRange)] =
    Parser(identifier ~ Index).map { case (id, end) =>
      id -> ps2tr(end - id.length, end)
    }

  def multiDotRef[_: Parser]: Parser[MultiDotRef] =
    Parser(identifierText ~ ("." ~ identifierText ~ !"(").rep).map { case (first, rest) =>
      MultiDotRef(first +: rest)
    }

//  implicit class StringOps(str: String) {
//    @inline
//    def t[_: Parser]: Parser[Token] = Parser(Index ~ str ~ Index).map { case (start, end) =>
//      new Token(str, ps2tr(start, end))
//    }
//  }

  // TODO figure out why this doesn't work
  // @inline
  // def binExpr[_ : Parser](ops: Parser[String], top: Parser[Expr], ctor: (Expr, String, Expr) => Expr = new BinaryExpr(_, _, _)) =
  //   Parser(top ~ (ops ~/ top).rep).map {
  //     case (left, reps) => println("foo!"); reps.foldLeft(left) {
  //       case (lhs, (op, rhs)) => ctor(lhs, op, rhs)
  //     }
  //   }

  // @inline
  // def argList[A, _: Parser](arg: Parser[A]): Parser[Seq[A]] = Parser(((arg ~ ",").rep ~ arg).?).map {
  //     case Some((firstArgs, lastArg)) => firstArgs :+ lastArg
  //     case _             => Nil
  //   }

  def GIVEN[_: Parser]: Parser[Unit] = Parser("given" ~~ !CharPred(_.isUnicodeIdentifierPart))
  def PROOF[_: Parser]: Parser[Unit] = Parser("proof" ~~ !CharPred(_.isUnicodeIdentifierPart))

  def modifier[_: Parser]: Parser[Modifier] = Parser(
    StringIn(
      "final",
      "public",
      "protected",
      "private",
      "synchronized",
      "transient",
      "volatile",
      "native",
      "const",
      "given",
      "proof",
      "default",
      "static",
      "abstract",
      "erased"
    ).! ~~ !CharPred(_.isUnicodeIdentifierPart) ~ Index
  ).map { case (modifier, end) =>
    Modifier(ModifierType.valueOf(modifier.toUpperCase), ps2tr(end - modifier.length, end))
  }
  def modifiers[_: Parser]: Parser[Seq[Modifier]] = Parser(modifier.rep)

  def annotation[_: Parser]: Parser[Annotation] = ???

  def dotPath[_: Parser]: Parser[DotPath] =
    Parser(identifierWithTextRange ~ ("." ~ identifierWithTextRange).rep).map {
      case (top, tr, rest) =>
        DotPath((top, tr) +: rest)
    }

  def packageStmt[_: Parser]: Parser[PackageStmt] = Parser(Index ~ "package" ~/ dotPath ~ ";").map {
    case (pkgTokStart, path) => PackageStmt(path, pkgTokStart)
  }

  def importStmt[_: Parser]: Parser[ImportStmt] =
    Parser(Index ~ "import" ~/ dotPath ~ ("." ~ "*" ~ Index).? ~ ";").map {
      case (imptTokStart, path, None) =>
        ImportStmt(
          path,
          TextRange(Parser.getPos(imptTokStart), path.textRange.end),
          wildCard = false
        )
      case (imptTokStart, path, Some(wildcardInd)) =>
        ImportStmt(path, ps2tr(imptTokStart, wildcardInd), wildCard = true)
    }

  // todo update list of hard keywords
  val hardKeywords: Set[String] = Set(
    "class",
    "interface",
    "enum",
    "new",
    "void",
    "throw",
    "return",
    "try",
    "catch",
    "if",
    "else",
    "while",
    "for",
    "do",
    "super",
    "this",
    "false",
    "true",
    "final",
    "public",
    "protected",
    "private",
    "synchronized",
    "transient",
    "volatile",
    "native",
    "const",
    "default",
    "static",
    "abstract"
  )
}
