import verity.compiler.ast.*

import verity.compiler.parser.Parser.tr

import cats.parse.{Parser, Parser0}
import cats.parse.Rfc5234.{sp, crlf, lf, wsp}
import cats.data.NonEmptyList

/** General purpose parsers
  */
private[parser] object Core {

  /** Whitespace */
  def ws: Parser0[Unit] = (sp | crlf | lf).rep0.void

  /** Lookahead sort of thing to make sure an identifier's ended */
  def idEnd: Parser0[Unit] = Parser.not(
    Parser.charWhere(_.isUnicodeIdentifierPart)
  )

  def identifier(id: String): Parser[Unit] = Parser.string(id) *> Parser.not(
    Parser.charWhere(_.isUnicodeIdentifierPart)
  )

  def identifier: Parser[String] =
    (Parser.charWhere(_.isUnicodeIdentifierStart) ~ Parser.charsWhile0(
      _.isUnicodeIdentifierPart
    )).map { case (first, rest) => s"$first$rest" }

  /** Like [[identifierWithTextRange]], but doesn't get inlined, and a tuple
    * doesn't have to be turned into a Text object later
    */
  def identifierText: Parser[Text] =
    (identifier ~ Parser.index).map { case (id, end) =>
      Text(id, tr(end - id.length, end))
    }

  /** Like [[identifierWithTextRange]], but can be inlined
    */
  def identifierWithTextRange: Parser[(String, TextRange)] =
    (identifier ~ Parser.index).map { case (id, end) =>
      id -> tr(end - id.length, end)
    }

  def withRange[A](parser: Parser[A]): Parser[(Int, A, Int)] =
    (Parser.index.with1 ~ parser ~ Parser.index).map { case ((start, a), end) =>
      (start, a, end)
    }

  val GIVEN: Parser[Unit] = identifier("given")
  val PROOF: Parser[Unit] = identifier("proof")

  val modifier: Parser[Modifier] = (Parser.stringIn(
    List(
      "final",
      "public",
      "protected",
      "private",
      "const",
      "given",
      "proof",
      "default",
      "static",
      "abstract",
      "erased"
    )
  ) ~ (idEnd *> Parser.index)).map { case (modifier, end) =>
    Modifier(
      modifier,
      tr(end - modifier.length, end)
    )
  }

  val dotPath: Parser[NonEmptyList[String]] =
    identifier.repSep(1, ws *> Parser.string0(".") *> ws)

  val packageStmt: Parser[PackageStmt] =
    (identifier("package") *> dotPath.surroundedBy(ws) <* Parser.string0(";"))
      .map { case path => PackageStmt(path) }

  val importStmt: Parser[ImportStmt] =
    (identifier("import") *> dotPath.surroundedBy(ws) ~ (Parser.string(
      "."
    ) *> ws *> Parser.string("*")).? <* Parser.string(";")).map {
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
}
