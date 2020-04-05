package com.ysthakur.parsing.grammar

import com.ysthakur.util._
import com.ysthakur.util.as
import com.ysthakur.util.utils

case class FunctionPattern[Input](
    matchFun: (_ <: Iterable[Input], Int) => MatchResult,
    override val isFixed: Boolean = false
) extends Pattern[Input] {
  import com.ysthakur.util.as
  override def tryMatch(input: Iterable[Input], offset: Int): MatchResult =
    matchFun(input.asInstanceOf, offset)
}

implicit def toFunctionPattern[Input]
  (matchFun: (_ <: Iterable[Input], Int) => MatchResult): FunctionPattern[Input] =
    FunctionPattern(matchFun)