package verity.parser

import fastparse.JavaWhitespace._
import fastparse._
import verity.ast._
import verity.ast.infile._
import verity.ast.infile.Unresolved._

private object Core {
  def identifier[_: P]: P[String] =
    P(CharPred(_.isUnicodeIdentifierStart).! ~ CharsWhile(_.isUnicodeIdentifierPart, 0).!).map {
      case (first, rest) => first + rest
    }

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple doesn't have to be
    * turned into a Text object later
    */
  def identifierText[_: P]: P[Text] =
    P(Index ~ identifier ~ Index).map { case (start, id, end) => Text(id, TextRange(start, end)) }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  def identifierWithTextRange[_: P]: P[(String, TextRange)] =
    P(Index ~ identifier ~ Index).map { case (start, id, end) =>
      id -> TextRange(start, end)
    }

  def multiDotRef[_: P]: P[MultiDotRef] = P(identifierText ~ ("." ~ identifierText ~ !"(").rep).map {
    case (first, rest) => MultiDotRef(first +: rest)
  }

//  implicit class StringOps(str: String) {
//    @inline
//    def t[_: P]: P[Token] = P(Index ~ str ~ Index).map { case (start, end) =>
//      new Token(str, TextRange(start, end))
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

  @inline
  def argList[A, _: P](arg: P[A]): P[Seq[A]] =
    P((arg ~ ",").rep ~ arg.?).map {
      case (firstArgs, Some(lastArg)) => firstArgs :+ lastArg
      case (firstArgs, _)             => firstArgs
    }

  def modifier[_: P]: P[Modifier] = P(
      Index ~ StringIn(
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
          "abstract"
      ).! ~ Index
  ).map { case (start, modifier, end) =>
    Modifier(ModifierType.valueOf(modifier.toUpperCase), TextRange(start, end))
  }
  def modifiers[_: P]: P[Seq[Modifier]] = P(modifier.rep)

  def annotation[_: P]: P[Annotation] = ???

  def dotPath[_: P]: P[DotPath] =
    P(identifierWithTextRange ~ ("." ~ identifierWithTextRange).rep).map { case (top, tr, rest) =>
      DotPath((top, tr) +: rest)
    }

  def packageStmt[_: P]: P[PackageStmt] = P(Index ~ "package" ~/ dotPath ~ ";").map {
    case (pkgTokStart, path) => new PackageStmt(path, pkgTokStart)
  }

  def importStmt[_: P]: P[ImportStmt] =
    P(Index ~ "import" ~/ dotPath ~ ("." ~ "*" ~ Index).? ~ ";").map {
      case (imptTokStart, path, None) =>
        ImportStmt(path, TextRange(imptTokStart, path.textRange.end), wildCard = false)
      case (imptTokStart, path, Some(wildcardInd)) =>
        ImportStmt(path, TextRange(imptTokStart, wildcardInd), wildCard = true)
    }
}
