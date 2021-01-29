package verity.parsing.parser

import verity.parsing.TextRange
import verity.parsing.Token
import verity.parsing.ast.infile.{EmptyNode, Node}
import verity.parsing.ast.ConsNode
// import verity.parsing.lexer.{Token, Tok}

sealed trait ParseResult[+N] {
  // def and[O >: N](other: => ParseResult[O]): ParseResult[O]
  def or[O](other: => ParseResult[O]): ParseResult[N | O]
  def map[R](f: N => R): ParseResult[R]
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
  override def map[R](f: N => R): Matched[R] = this.copy(create=() => f(create()))
}

object Matched {
  // def empty(rest: Reader, range: TextRange, pattern: Option[Pattern] = None) =
  //   new Matched(() => EmptyNode(range.start), rest, range, pattern)

  // def apply[N](
  //     create: => N,
  //     rest: Reader,
  //     range: TextRange
  // ): Matched[N] = new Matched(() => create, rest, range)
//  def unapply[N](matched: Matched[N]): (() => N, Reader, TextRange) =
//    (matched.create, matched.rest, matched.range)
}

case class Failed(
    got: Token,
    expected: Iterable[String],
    pos: Int,
    canBacktrack: Boolean
    // override val pattern: Option[Pattern] = None
) extends ParseResult[Nothing] {
  // override def and[O](other: => ParseResult[O]): ParseResult[O] = this
  override def or[O](other: => ParseResult[O]): ParseResult[O] = other
  override def map[R](f: Nothing => R): Failed = this
}

//object Failed {
//  def unapply(arg: Failed): (Token, Iterable[String], Int) =
//    (arg.got, arg.expected, arg.pos)
//}