package verity.compiler.parser

import verity.compiler.ast.*

import verity.compiler.parser.Parser.tr

import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.{sp, crlf, lf, wsp}
import cats.data.NonEmptyList

/** General purpose parsers
  */
private[parser] object Core {

  /** Whitespace */
  def ws: P0[Unit] = (sp | crlf | lf).rep0.void

  /** Lookahead sort of thing to make sure an identifier's ended */
  def idEnd: P0[Unit] = P.not(
    P.charWhere(_.isUnicodeIdentifierPart)
  )

  def identifier(id: String): P[Unit] = P.string(id) *> P.not(
    P.charWhere(_.isUnicodeIdentifierPart)
  )

  def identifier: P[String] =
    (P.charWhere(_.isUnicodeIdentifierStart) ~ P.charsWhile0(
      _.isUnicodeIdentifierPart
    )).map { case (first, rest) => s"$first$rest" }

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple
    * doesn't have to be turned into a Text object later
    */
  def identifierText: P[Text] =
    (identifier ~ P.index).map { case (id, end) =>
      Text(id, tr(end - id.length, end))
    }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  def identifierWithTextRange: P[(String, TextRange)] =
    (identifier ~ P.index).map { case (id, end) =>
      id -> tr(end - id.length, end)
    }

  def withRange[A](parser: P[A]): P[(Int, A, Int)] =
    (P.index.with1 ~ parser ~ P.index).map { case ((start, a), end) =>
      (start, a, end)
    }

  val GIVEN: P[Unit] = identifier("given")
  val PROOF: P[Unit] = identifier("proof")

  val modifier: P[Modifier] = (P.stringIn(
    List(
      "pub",
      "mut",
      "const",
      "given"
    )
  ) ~ (idEnd *> P.index)).map { case (modifier, end) =>
    Modifier(modifier, tr(end - modifier.length, end))
  }

  val dotPath: P[NonEmptyList[String]] =
    identifier.repSep(1, ws *> P.string0(".") *> ws)

  val packageStmt: P[PackageStmt] =
    (identifier("package") *> dotPath.surroundedBy(ws) <* P.string0(";")).map {
      case path => PackageStmt(path)
    }

  val importStmt: P[ImportStmt] =
    (identifier("import") *> dotPath.surroundedBy(ws) ~ (P.string(
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
