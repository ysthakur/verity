package verity.compiler.parser

import verity.compiler.ast.*

import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.{sp, crlf, lf, wsp}
import cats.data.NonEmptyList

/** General purpose parsers
  */
private[parser] object Core {

  def pos: P0[Pos] = P.caret.map { caret =>
    Pos(caret.offset, caret.line, caret.col)
  }

  extension [A](p1: P[A])
    /** Like `~`, but allows whitespace in between */
    def ~~[B](p2: P0[B]): P[(A, B)] = (p1 <* ws) ~ p2

  extension [A](p1: P0[A])
    /** Like `~`, but allows whitespace in between */
    def ~~[B](p2: P0[B]): P0[(A, B)] = (p1 <* ws) ~ p2

  /** Whitespace */
  val ws: P0[Unit] = (sp | crlf | lf).rep0.void

  /** Lookahead sort of thing to make sure an identifier's ended */
  val idEnd: P0[Unit] = P.not(
    P.charWhere(_.isUnicodeIdentifierPart)
  )

  def keyword(id: String): P[Unit] = P.string(id) *> P.not(
    P.charWhere(_.isUnicodeIdentifierPart)
  )

  val identifier: P[String] =
    (P.charWhere(_.isUnicodeIdentifierStart) ~ P.charsWhile0(
      _.isUnicodeIdentifierPart
    )).map { case (first, rest) => s"$first$rest" }

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple
    * doesn't have to be turned into a Text object later
    */
  val identifierText: P[Text] =
    (identifier ~ pos).map { case (id, end) =>
      Text(id, Span(end - id.length, end))
    }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  val identifierWithTextRange: P[(String, Span)] =
    (identifier ~ pos).map { case (id, end) =>
      id -> Span(end - id.length, end)
    }

  def withRange[A](parser: P[A]): P[(Pos, A, Pos)] =
    (pos.with1 ~ parser ~ pos).map { case ((start, a), end) =>
      (start, a, end)
    }

  val GIVEN: P[Unit] = keyword("given")
  val PROOF: P[Unit] = keyword("proof")

  val modifier: P[Modifier] = (P.stringIn(
    List(
      "pub",
      "mut",
      "const",
      "given"
    )
  ) ~ (idEnd *> pos)).map { case (modifier, end) =>
    Modifier(modifier, Span(end - modifier.length, end))
  }

  val dotPath: P[NonEmptyList[String]] =
    identifier.repSep(1, ws *> P.string0(".") *> ws)

  val importStmt: P[ImportStmt] =
    (keyword("import") *> dotPath.surroundedBy(ws) ~ (P.string(
      "."
    ) *> ws *> P.string("*")).? <* P.string(";")).map {
      case (path, None) =>
        ImportStmt(path, wildcard = false)
      case (path, Some(_)) =>
        ImportStmt(path, wildcard = true)
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
    "const",
    "default",
    "static",
    "abstract"
  )

  /** Helper to make parser for argument or parameter lists */
  def list[T](
    start: P[Unit],
    item: P[T],
    sep: P[Unit],
    end: P[Unit]
  ): P[List[T]] =
    start.soft *> ws *> item.repSep0(ws *> sep <* ws) <* ws <* end
}
