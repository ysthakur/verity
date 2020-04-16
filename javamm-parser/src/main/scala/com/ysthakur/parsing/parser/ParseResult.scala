package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.{ConsNode, Node}

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult = if (this.isInstanceOf[Failed]) res else this
  def +(other: => ParseResult): ParseResult
  def pattern: Option[Pattern]
}

class Matched[N <: Node, T](val create: () => N | Null,
                            val rest: List[Node],
                            val offset: Int,
                            val isEmpty: Boolean = false,
                            override val pattern: Option[Pattern] = None)
    extends ParseResult {
  override def +(other: => ParseResult): ParseResult = other match {
    case Matched(create2, rest2, offset2) =>
      Matched(ConsNode(create(), create2()), rest2, offset2)
    case _ => this
  }
}
object Matched {
  def empty(rest: List[Node], pattern: Option[Pattern] = None) = 
    new Matched(() => null, rest, 0, true, pattern)
  
  def apply[N <: Node, T](create: => N | Null, rest: List[Node], offset: Int): Matched[N, T] =
    new Matched(() => create, rest, offset)
  def unapply[N <: Node, T](matched: Matched[N, T]): (() => N | Null, List[Node], Int) =
    (matched.create, matched.rest, matched.offset)
}

case class Failed(got: Any,
                  expected: Iterable[String]/* = List.empty*/,
                  override val pattern: Option[Pattern] = None) extends ParseResult {
  override def +(other: => ParseResult): ParseResult = this
}

object Failed {
  def unapply(arg: Failed): Option[(Any, Iterable[String])] = Some((arg.got, arg.expected))
}