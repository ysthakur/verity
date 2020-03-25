package com.ysthakur.parsing

case class PredicatePattern[Input](matchFun: (_ <: Iterable[Input]) => MatchResult,
                                   override val isFixed: Boolean = false)
    extends Pattern[Input] {
    override def tryMatch[T <: Iterable[Input]](input: T): MatchResult = matchFun(input.asInstanceOf)
}
