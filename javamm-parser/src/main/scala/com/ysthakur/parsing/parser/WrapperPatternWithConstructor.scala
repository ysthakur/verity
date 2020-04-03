package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{Match, MatchResult, Pattern}
import com.ysthakur.parsing.parser.NodeCtor
import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.grammar.PatternMatch

abstract class PatternWithConstructor[M <: Match[?], N](
  val ctor: M => N
) extends Pattern[Node]

case class WrapperPatternWithConstructor[M <: Match[?], N](
  pattern: Pattern[Node],
  override val ctor: M => N
) extends PatternWithConstructor[M, N](ctor) {
  override val isFixed: Boolean = pattern.isFixed
  override def tryMatch(input: Iterable[Node]): MatchResult =
    pattern.tryMatch(input)
}

case class ApplicablePattern[P <: Pattern[Node]](pattern: P) {
  def apply[M <: Match[?], N](ctor: M => N): WrapperPatternWithConstructor[M, N] = 
    WrapperPatternWithConstructor(pattern, ctor)
  def >>[M <: PatternMatch[pattern.I], N](
    ctor: M => N
  ): WrapperPatternWithConstructor[M, N] = WrapperPatternWithConstructor(pattern, ctor)
}

implicit def toApplicablePattern[P <: Pattern[Node]](pattern: P): ApplicablePattern[P] = ApplicablePattern(pattern)