package verity.parser

import verity.ast._, infile._

import fastparse._, JavaWhitespace._

private object Core {
  def identifier[_: P]: P[String] =
    P(CharPred(_.isUnicodeIdentifierStart).! ~ CharsWhile(_.isUnicodeIdentifierPart, 0).!).map {
      case (first, rest) => first + rest
    }
  /**
   * Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple doesn't have to be
   * turned into a Text object later
   */
  def identifierText[_: P]: P[Text] =
    P(Index ~ identifier ~ Index).map { case (start, id, end) => Text(id, TextRange(start, end)) }
  /**
   * Like [[identifierWithTextRange]], but can be inlined
   */
  def identifierWithTextRange[_: P]: P[(String, TextRange)] =
    P(Index ~ identifier ~ Index).map { case (start, id, end) =>
      id -> TextRange(start, end)
    }

  //todo clear this up?
  def upperBound[_: P] = P("extends" ~/ typeRef)
  def lowerBound[_: P] = P("super" ~/ typeRef)
  // def typeBound[_: P] = P(Index ~ StringIn("super", "extends").! ~/ Index ~ typeRef)

  def typeRef[_: P]: P[TypeRef] = P(Index ~ identifier ~ Index ~ ("<" ~ Index ~/ typeArgList ~ ">" ~ Index).?).map {
    case (nameStart, name, nameEnd, Some((argStart, args, argEnd))) =>
      new TypeRef(Text(name, TextRange(nameStart, nameEnd)), args, None, TextRange(argStart, argEnd))
    case (nameStart, name, nameEnd, None) =>
      new TypeRef(Text(name, TextRange(nameStart, nameEnd)), List.empty, None, TextRange.synthetic)
  }
  def wildCard[_: P]: P[Wildcard] =
    P("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map { case (upper, lower) =>
      Wildcard(upper, lower)
    }
  def typeArg[_: P]: P[Type] = P(wildCard | typeRef)
  def typeArgList[_: P] = argList(typeArg: P[Type])

  def typeParam[_: P] = P(identifierWithTextRange ~ upperBound.? ~ lowerBound.?).map {
    case (name, nameRange, upper, lower) =>
      new TypeParam(name, upper.getOrElse(BuiltinTypes.objectType), lower.getOrElse(NothingType), nameRange)
  }
  def typeParamList[_: P] = P("<" ~/ Index ~ typeParam ~ ("," ~ typeParam).rep ~ ">" ~ Index).map {
    case (start, first, rest, end) => new TypeParamList(first +: rest, TextRange(start, end))
  }

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
          "given",
          "proof",
          "default",
          "static",
          "abstract"
      ).! ~ Index
  ).map { case (start, modifier, end) =>
    Modifier(ModifierType.valueOf(modifier.toUpperCase), TextRange(start, end))
  }
  def modifiers[_: P] = P(modifier.rep)

  def annotation[_: P]: P[Annotation] = ???

  def dotPath[_: P] = P(identifierWithTextRange ~ ("." ~ identifierWithTextRange).rep).map {
    case (top, tr, rest) => DotPath((top, tr) +: rest)
  }

  def packageStmt[_: P]: P[PackageStmt] = P(Index ~ "package" ~/ dotPath ~ ";").map {
    case (pkgTokStart, path) => new PackageStmt(path, pkgTokStart)
  }

  def importStmt[_: P]: P[ImportStmt] =
    P(Index ~ "import" ~/ dotPath ~ ("." ~ "*" ~ Index).? ~ ";").map {
      case (imptTokStart, path, None) =>
        new ImportStmt(path, TextRange(imptTokStart, path.textRange.end), false)
      case (imptTokStart, path, Some(wildcardInd)) =>
        new ImportStmt(path, TextRange(imptTokStart, wildcardInd), true)
    }
}
