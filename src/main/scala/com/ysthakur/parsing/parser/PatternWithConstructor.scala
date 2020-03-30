package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{Match, MatchResult, Pattern}

case class PatternWithConstructor[Input, M <: Match[Input]](
    pattern: Pattern[Input],
    constructor: M => Node
) extends Pattern[Input] {
  override val isFixed: Boolean = pattern.isFixed
  override def tryMatch(input: Iterable[Input]): MatchResult = pattern.tryMatch(input)
}
