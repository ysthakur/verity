package verity.parsing.parser

import verity.parsing.TextRange
import verity.parsing.Token
import verity.parsing.ast.infile.{EmptyNode, Node}
import verity.parsing.ast.ConsNode
// import verity.parsing.lexer.{Token, Tok}

sealed trait ParseResult[+N] {
  // def and[O >: N](other: => ParseResult[O]): ParseResult[O]
  def or[O](other: => ParseResult[O]): ParseResult[N | O]
  // def pattern: Option[Pattern]
}

case class Matched[+N](
    val create: () => N,
    val rest: Reader,
    val range: verity.parsing.TextRange
) extends ParseResult[N] {
  // override def and[O >: N](other: => ParseResult[O]): ParseResult[O] = other match {
  //   case Matched(create2, rest2, offset2) =>
  //     Matched(() => ConsNode(create(), create2()), rest2, offset2)
  //   case _ => this
  // }
  override def or[O](other: => ParseResult[O]): ParseResult[N] = this
}

object Matched {
  // def empty(rest: Reader, range: TextRange, pattern: Option[Pattern] = None) =
  //   new Matched(() => EmptyNode(range.start), rest, range, pattern)

  // def apply[N](
  //     create: => N,
  //     rest: Reader,
  //     range: TextRange
  // ): Matched[N] = new Matched(() => create, rest, range)
  def unapply[N](matched: Matched[N]): (() => N, Reader, TextRange) =
    (matched.create, matched.rest, matched.range)
}

case class Failed(
    got: Token,
    expected: Iterable[String],
    pos: Int
    // override val pattern: Option[Pattern] = None
) extends ParseResult[Nothing] {
  // override def and[O](other: => ParseResult[O]): ParseResult[O] = this
  override def or[O](other: => ParseResult[O]): ParseResult[O] = other
}

object Failed {
  def unapply(arg: Failed): Some[(Token, Iterable[String], Int)] =
    Some((arg.got, arg.expected, arg.pos))
}