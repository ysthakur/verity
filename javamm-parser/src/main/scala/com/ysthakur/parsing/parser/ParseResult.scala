package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Node

sealed trait ParseResult

class Matched[N <: Node, T](val created: () => N, val rest: List[Node], val offset: Int)
    extends ParseResult
object Matched {
  def apply[N <: Node, T](created: => N, rest: List[Node], offset: Int): Matched[N, T] = 
    new Matched(() => created, rest, offset)
  def unapply[N <: Node, T](matched: Matched[N, T]): Option[(() => N, List[Node], Int)] =
    Some(matched.created, matched.rest, matched.offset)
}

object Failed extends ParseResult