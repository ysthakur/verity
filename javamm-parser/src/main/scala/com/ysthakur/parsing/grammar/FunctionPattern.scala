package com.ysthakur.parsing.grammar

import com.ysthakur.util._
import com.ysthakur.util.as
import com.ysthakur.util.utils

case class FunctionPattern[Input](
    matchFun: (_ <: Iterable[Input]) => MatchResult,
    override val isFixed: Boolean = false
) extends Pattern[Input] {
  import com.ysthakur.util.as
  override def tryMatch(input: Iterable[Input]): MatchResult =
    matchFun(input.asInstanceOf)
}

implicit def toFunctionPattern[Input]
  (matchFun: (_ <: Iterable[Input]) => MatchResult): FunctionPattern[Input] =
    FunctionPattern(matchFun)