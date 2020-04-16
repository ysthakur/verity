package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.{ConsNode, Node}

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult = if (this == Failed()) res else this
  def +(other: ParseResult): ParseResult
}

class Matched[N <: Node, T](val create: () => N | Null,
                            val rest: List[Node],
                            val offset: Int,
                            val isEmpty: Boolean = false)
    extends ParseResult {
  override def +(other: ParseResult): ParseResult = other match {
    case Matched(create2, rest2, offset2) => 
      Matched(ConsNode(create(), create2()), rest2, offset2)
    case _ => this
  }
}
object Matched {
  def apply[N <: Node, T](create: => N | Null, rest: List[Node], offset: Int): Matched[N, T] = 
    new Matched(() => create, rest, offset)
  def unapply[N <: Node, T](matched: Matched[N, T]): (() => N | Null, List[Node], Int) =
    (matched.create, matched.rest, matched.offset)
}

case class Failed(reason: String = "Failed") extends ParseResult {
  override def +(other: ParseResult): ParseResult = Failed()
}