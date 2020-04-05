package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar._
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing._

case class PatternAndConstructor[M <: Match[?], N](
  pattern: Pattern[Node],
  override val ctor: M => N
) extends Pattern[N] {
  override val isFixed: Boolean = pattern.isFixed
  override def tryMatch(input: Iterable[Node], offset: Int): MatchResult =
    pattern.tryMatch(input, offset)
}

case class ApplicablePattern[P <: Pattern[Node]](pattern: P) {
  def apply[M <: Match[?], N](ctor: M => N): PatternAndConstructor[M, N] = 
    PatternAndConstructor(pattern, ctor)
  // def >>[M <: PatternMatch[pattern.I], N](
  //   ctor: M => N
  // ): PatternAndConstructor[M, N] = PatternAndConstructor(pattern, ctor)
}

implicit def toApplicablePattern[P <: Pattern[Node]](pattern: P): ApplicablePattern[P] = ApplicablePattern(pattern)