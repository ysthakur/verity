package verity.parser

import verity.ast._
import verity.ast.infile._
import verity.ast.infile.unresolved._
import Parser.ps2tr

import fastparse.JavaWhitespace._
import fastparse._

private class Core(implicit offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]) {

  /** Negative lookahead to ensure that an identifier has ended
    */
  def nid[_: P] = P(!CharPred(_.isUnicodeIdentifierPart))

  def identifier[_: P]: P[String] =
    P(CharPred(_.isUnicodeIdentifierStart).! ~~ CharsWhile(_.isUnicodeIdentifierPart, 0).!)
      .map { case (first, rest) => first + rest }
      .filter(s => !hardKeywords(s))

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple doesn't have to be
    * turned into a Text object later
    */
  def identifierText[_: P]: P[Text] =
    P(identifier ~ Index).map { case (id, end) => Text(id, ps2tr(end - id.length, end)) }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  def identifierWithTextRange[_: P]: P[(String, TextRange)] =
    P(identifier ~ Index).map { case (id, end) =>
      id -> ps2tr(end - id.length, end)
    }

  def multiDotRef[_: P]: P[MultiDotRef] =
    P(identifierText ~ ("." ~ identifierText ~ !"(").rep).map { case (first, rest) =>
      MultiDotRef(first +: rest)
    }

//  implicit class StringOps(str: String) {
//    @inline
//    def t[_: P]: P[Token] = P(Index ~ str ~ Index).map { case (start, end) =>
//      new Token(str, ps2tr(start, end))
//    }
//  }

  //TODO figure out why this doesn't work
  // @inline
  // def binExpr[_ : P](ops: P[String], top: P[Expr], ctor: (Expr, String, Expr) => Expr = new BinaryExpr(_, _, _)) =
  //   P(top ~ (ops ~/ top).rep).map {
  //     case (left, reps) => println("foo!"); reps.foldLeft(left) {
  //       case (lhs, (op, rhs)) => ctor(lhs, op, rhs)
  //     }
  //   }

  // @inline
  // def argList[A, _: P](arg: P[A]): P[Seq[A]] = P(((arg ~ ",").rep ~ arg).?).map {
  //     case Some((firstArgs, lastArg)) => firstArgs :+ lastArg
  //     case _             => Nil
  //   }

  def GIVEN[_: P]: P[Unit] = P("given" ~~ !CharPred(_.isUnicodeIdentifierPart))
  def PROOF[_: P]: P[Unit] = P("proof" ~~ !CharPred(_.isUnicodeIdentifierPart))

  def modifier[_: P]: P[Modifier] = P(
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
  def modifiers[_: P]: P[Seq[Modifier]] = P(modifier.rep)

  def annotation[_: P]: P[Annotation] = ???

  def dotPath[_: P]: P[DotPath] =
    P(identifierWithTextRange ~ ("." ~ identifierWithTextRange).rep).map { case (top, tr, rest) =>
      DotPath((top, tr) +: rest)
    }

  def packageStmt[_: P]: P[PackageStmt] = P(Index ~ "package" ~/ dotPath ~ ";").map {
    case (pkgTokStart, path) => PackageStmt(path, pkgTokStart)
  }

  def importStmt[_: P]: P[ImportStmt] =
    P(Index ~ "import" ~/ dotPath ~ ("." ~ "*" ~ Index).? ~ ";").map {
      case (imptTokStart, path, None) =>
        ImportStmt(
          path,
          TextRange(Parser.getPos(imptTokStart), path.textRange.end),
          wildCard = false
        )
      case (imptTokStart, path, Some(wildcardInd)) =>
        ImportStmt(path, ps2tr(imptTokStart, wildcardInd), wildCard = true)
    }

  //todo update list of hard keywords
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
