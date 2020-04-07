package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing._

case class PatternAndConstructor[M <: Match[?], N <: Node](
  pattern: Pattern,
  val ctor: M => N
) extends Pattern {
  override type AsNode = N
  override type Input = pattern.Input

  override val isFixed: Boolean = pattern.isFixed
  override val isEager: Boolean = pattern.isEager
  override def tryMatch(input: Iterable[Input], offset: Int): MatchResult =
    pattern.tryMatch(input, offset)
  override def create(matched: MatchIn): this.AsNode = ctor(matched.asInstanceOf)
  override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
}

// case class ApplicablePattern[P <: Pattern](pattern: P) {
//   def apply[M <: Match[?], N <: Node](ctor: M => N): PatternAndConstructor[M, N] = 
//     PatternAndConstructor[M, N](pattern, ctor)
//   // def >>[M <: PatternMatch[pattern.I], N](
//   //   ctor: M => N
//   // ): PatternAndConstructor[M, N] = PatternAndConstructor(pattern, ctor)
// }

implicit def toApplicablePattern[P <: Pattern, M <: Match[?], N <: Node](
    pattern: P
  ): (M => N) => PatternAndConstructor[M, N] = PatternAndConstructor[M, N](pattern, _)