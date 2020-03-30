package com.ysthakur.parsing.grammar

import com.ysthakur.util._

case class FunctionPattern[Input](
    matchFun: (_ <: Iterable[Input]) => MatchResult,
    override val isFixed: Boolean = false)
    extends Pattern[Input] {
  override def tryMatch(input: Iterable[Input]): MatchResult =
    matchFun(input.as)
}
