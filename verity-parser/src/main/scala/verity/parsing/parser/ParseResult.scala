package verity.parsing.parser

import verity.parsing.TextRange
import verity.parsing.Token
import verity.parsing.ast.infile.{EmptyNode, Node}
import verity.parsing.ast.ConsNode
// import verity.parsing.lexer.{Token, Tok}

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult
  def +(other: => ParseResult): ParseResult
  def pattern: Option[Pattern]
}

case class Matched[N <: Node, T](
    val create: () => N,
    val rest: Reader,
    val range: verity.parsing.TextRange,
    override val pattern: Option[Pattern] = None
) extends ParseResult {
  override def +(other: => ParseResult): ParseResult = other match {
    case Matched(create2, rest2, offset2) =>
      Matched(() => ConsNode(create(), create2()), rest2, offset2)
    case _ => this
  }
  override def orElse(other: => ParseResult) = this
}

object Matched {
  def empty(rest: Reader, range: TextRange, pattern: Option[Pattern] = None) =
    new Matched(() => EmptyNode(range.start), rest, range, pattern)

  def apply[N <: Node, T](
      create: => N,
      rest: Reader,
      range: TextRange
  ): Matched[N, T] = new Matched(() => create, rest, range)
  def unapply[N <: Node, T](matched: Matched[N, T]): (() => N, Reader, TextRange) =
    (matched.create, matched.rest, matched.range)
}

case class Failed(
    got: Token,
    expected: Iterable[String],
    pos: Int,
    override val pattern: Option[Pattern] = None
) extends ParseResult {
  override def +(other: => ParseResult): ParseResult = this
  override def orElse(other: => ParseResult) = other
}

object Failed {
  def unapply(arg: Failed): Some[(Token, Iterable[String], Int)] =
    Some((arg.got, arg.expected, arg.pos))
}