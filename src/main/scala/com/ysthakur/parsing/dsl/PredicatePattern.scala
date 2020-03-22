package com.ysthakur.parsing.dsl

import com.ysthakur.parsing.MatchResult

case class PredicatePattern[Input](matchFun: (_ <: Iterable[Input]) => MatchResult)
    extends Pattern[Input] {
    override def tryMatch[T <: Iterable[Input]](input: T): MatchResult = matchFun(input.asInstanceOf)
}
