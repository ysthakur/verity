package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._

import fastparse._, JavaWhitespace._

private object Core {
  def identifier[_: P]: P[Name] =
    P(
        Index ~ CharPred(_.isUnicodeIdentifierStart).! ~ CharsWhile(
            _.isUnicodeIdentifierPart
        ).! ~ Index
    ).map {
      case (start, first, rest, end) =>
        Name(first + rest, TextRange(start, end))
    }

  def typeRef[_: P]: P[TypeRef] = P(identifier ~ "<" ~ typeArgList ~ ">").map {
    case (name, args) => TypeRef(name, args)
  }
  def wildCard[_: P]: P[Wildcard] =
    P("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map {
      case (upper, lower) => Wildcard(upper, lower)
    }
  def typeArg[_: P]: P[Type] = P(wildCard | typeRef)
  def typeArgList[_: P] = argList(typeArg: P[Type])

  implicit class StringOps(str: String) {
    @inline
    def t[_: P] = P(Index ~ str ~ Index).map {
      case (start, end) => new Token(str, TextRange(start, end))
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
  ).map {
    case (start, modifier, end) => Modifier(ModifierType.valueOf(modifier), TextRange(start, end))
  }
  def modifiers[_: P] = P(modifier.rep)

  def annotation[_: P]: P[Annotation] = ???
}
