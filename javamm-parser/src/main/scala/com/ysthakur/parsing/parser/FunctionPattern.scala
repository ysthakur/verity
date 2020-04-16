package com.ysthakur.parsing.parser

import com.ysthakur.util._
import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.Match

import scala.Option
import scala.collection.mutable.ListBuffer

case class FunctionPattern[M <: Match[?], N <: Node](
    matchFun: (List[Node], Int) => ParseResult,
    override val isFixed: Boolean = false,
    override val isEager: Boolean = true
) extends Pattern {
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult =
    matchFun(input, offset)
  override def ==(other: Pattern): Boolean = this.equals(other)
}