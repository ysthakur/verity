package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.parsing.{Position, TextRange}
import com.ysthakur.verity.parsing.ast.infile.{EmptyNode, Node}
import com.ysthakur.verity.parsing.ast.ConsNode
import com.ysthakur.verity.parsing.lexer.{Token, Tok}

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult =
    if (this.isInstanceOf[Failed]) res else this
  def +(other: => ParseResult): ParseResult
  def pattern: Option[Pattern]
}

case class Matched[N <: Node, T](
    val create: () => N,
    val rest: List[Tok],
    val range: TextRange,
    val isEmpty: Boolean = false,
    override val pattern: Option[Pattern] = None
) extends ParseResult {
  override def +(other: => ParseResult): ParseResult = other match {
    case Matched(create2, rest2, offset2) =>
      Matched(() => ConsNode(create(), create2()), rest2, offset2)
    case _ => this
  }
}

object Matched {
  def empty(rest: List[Tok], range: TextRange, pattern: Option[Pattern] = None) =
    new Matched(() => EmptyNode, rest, range, true, pattern)

  def apply[N <: Node, T](
      create: => N,
      rest: List[Tok],
      range: TextRange
  ): Matched[N, T] = new Matched(() => create, rest, range)
  def unapply[N <: Node, T](matched: Matched[N, T]): (() => N, List[Tok], TextRange) =
    (matched.create, matched.rest, matched.range)
}

case class Failed(
    got: Token[?],
    expected: Iterable[String],
    pos: Position,
    override val pattern: Option[Pattern] = None
) extends ParseResult {
  override def +(other: => ParseResult): ParseResult = this
}

object Failed {
  def unapply(arg: Failed): Option[(Token[?], Iterable[String], Position)] =
    Some((arg.got, arg.expected, arg.pos))
}
