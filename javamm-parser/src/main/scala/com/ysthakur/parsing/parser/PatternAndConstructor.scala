package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing._
import com.ysthakur.parsing.lexer.Match

case class PatternAndConstructor[M <: Match[?], N <: Node](pattern: Pattern) extends Pattern {
  override type AsNode = N
  override type Input = pattern.Input

  override def isFixed: Boolean = pattern.isFixed
  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult =
    pattern.tryMatch(input, offset, trace)
  override def expected(prevRes: ParseResult): List[String] = pattern.expected(prevRes)
}

implicit def toApplicablePattern[P <: Pattern, M <: Match[?], N <: Node](
    pattern: P
  ): (M => N) => PatternAndConstructor[M, N] = PatternAndConstructor[M, N](pattern)