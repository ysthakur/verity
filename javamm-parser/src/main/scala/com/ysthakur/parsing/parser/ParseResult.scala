package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.infile.TextNode
import com.ysthakur.parsing.ast.{ConsNode, Node}
import com.ysthakur.parsing.lexer.{Position, Tok, Token}

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult =
    if (this.isInstanceOf[Failed]) res else this
  def +(other: => ParseResult): ParseResult
  def pattern: Option[Pattern]
}

class Matched[N <: TextNode, T](
    val create: () => N | Null,
    val rest: List[Tok],
    val offset: Int,
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
  def empty(rest: List[Tok], pattern: Option[Pattern] = None) =
    new Matched(() => null, rest, 0, true, pattern)

  def apply[N <: TextNode, T](
      create: () => N | Null,
      rest: List[Tok],
      offset: Int
  ): Matched[N, T] =
    new Matched(create, rest, offset)
  def unapply[N <: TextNode, T](
      matched: Matched[N, T]
  ): (() => N | Null, List[Tok], Int) =
    (matched.create, matched.rest, matched.offset)
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
