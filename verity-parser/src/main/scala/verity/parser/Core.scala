package verity.parsing.parser

import language.implicitConversions

import verity.ast._, infile._

import fastparse._, JavaWhitespace._

private object Core {
  def identifier[_: P]: P[String] =
    P(CharPred(_.isUnicodeIdentifierStart).! ~ CharsWhile(_.isUnicodeIdentifierPart, 0).!).map {
      case (first, rest) => first + rest
    }
  def identifierWithTextRange[_: P]: P[(String, TextRange)] =
    P(Index ~ identifier ~ Index).map { case (start, id, end) =>
      id -> TextRange(start, end)
    }

  def typeRef[_: P]: P[TypeRef] = P(Index ~ identifier ~ ("<" ~/ typeArgList ~ ">").? ~ Index).map {
    case (start, name, args, end) =>
      new TypeRef(name, args.getOrElse(List.empty), TextRange(start, end))
  }
  def wildCard[_: P]: P[Wildcard] =
    P("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map { case (upper, lower) =>
      Wildcard(upper, lower)
    }
  def typeArg[_: P]: P[Type] = P(wildCard | typeRef)
  def typeArgList[_: P] = argList(typeArg: P[Type])

  implicit class StringOps(str: String) {
    @inline
    def t[_: P] = P(Index ~ str ~ Index).map { case (start, end) =>
      new Token(str, TextRange(start, end))
    }
  }

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

  def modifier[_: P] = P(
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
          "default",
          "static",
          "abstract"
      ).! ~ Index
  ).map { case (start, modifier, end) =>
    Modifier(ModifierType.valueOf(modifier.toUpperCase), TextRange(start, end))
  }
  def modifiers[_: P] = P(modifier.rep)

  def annotation[_: P]: P[Annotation] = ???

  def dotRef[_: P]: P[DotRef] = P(Index ~ identifier ~ ("." ~ identifier).rep).map {
    case (top, rest) => new DotRef(top +: rest)
  }

  def packageStmt[_: P]: P[PackageStmt] = P(Index ~ "package" ~/ dotRef ~ ";").map {
    case (pkgTokStart, path) => new PackageStmt(path, pkgTokStart)
  }

  def importStmt[_: P]: P[ImportStmt] =
    P(Index ~ "import" ~/ dotRef ~ ("." ~ "*" ~ Index).? ~ ";").map {
      case (imptTokStart, path, None) =>
        new ImportStmt(path, TextRange(imptTokStart, path.textRange.end), false)
      case (imptTokStart, path, Some(wildcardInd)) =>
        new ImportStmt(path, TextRange(imptTokStart, wildcardInd), true)
    }
}
