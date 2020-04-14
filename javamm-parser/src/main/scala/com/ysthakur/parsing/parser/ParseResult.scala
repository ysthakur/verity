package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Node

sealed trait ParseResult {
  def orElse(res: => ParseResult): ParseResult = if (this == Failed) res else this
}

class Matched[N <: Node, T](val created: () => N | Null, 
                            val rest: List[Node], 
                            val offset: Int, 
                            val isEmpty: Boolean = false)
    extends ParseResult
object Matched {
  def apply[N <: Node, T](created: => N | Null, rest: List[Node], offset: Int): Matched[N, T] = 
    new Matched(() => created, rest, offset)
  def unapply[N <: Node, T](matched: Matched[N, T]): Option[(() => N | Null, List[Node], Int)] =
    Some(matched.created, matched.rest, matched.offset)
}

object Failed extends ParseResult