package com.ysthakur.parsing.dsl

import com.ysthakur.parsing.{MatchResult, PartialMatch, TextRange}

trait Pattern[Input] {

    def &(other: Pattern[Input]): Pattern[Input] = CompositePattern(this, other)

    def -->(action: => Unit): PatternCase[Input] = PatternCase(this, () => action)

    def tryMatch[T <: Iterable[Input]](input: T): MatchResult

}

case class CompositePattern[Input](pattern1: Pattern[Input], pattern2: Pattern[Input]) extends Pattern[Input] {
    override def &(other: Pattern[Input]): Pattern[Input] = CompositePattern(this, other)

    override def tryMatch[T <: Iterable[Input]](input: T): MatchResult = {
        val firstMatch = pattern1.tryMatch(input)
        firstMatch match {
            case PartialMatch(tr: TextRange) =>
                val secondMatch = pattern2.tryMatch(input.slice(tr.start, tr.end))
                secondMatch match {
                    case PartialMatch(tr2) => PartialMatch(TextRange(tr.start, tr2.end))
                    case _ => secondMatch
                }
            case _ => firstMatch
        }
    }
}