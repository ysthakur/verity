package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{Match, MatchResult, Pattern}

abstract class PatternWithConstructor[Input, M <: Match[Input], N <: Node](
    val ctor: NodeCtor[M, N]
) extends Pattern[ctor.Input]

case class WrapperPatternWithConstructor[C <: NodeCtor[_, _]](
    pattern: Pattern[C#Input],
    override val ctor: C
) extends PatternWithConstructor[ctor.Input, ctor.In, ctor.This](ctor) {
  override val isFixed: Boolean = pattern.isFixed
  override def tryMatch(input: Iterable[C#Input]): MatchResult =
    pattern.tryMatch(input)
}
