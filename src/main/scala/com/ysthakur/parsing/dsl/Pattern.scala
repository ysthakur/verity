package com.ysthakur.parsing.dsl

import com.ysthakur.parsing.{FullMatch, MatchResult, NeedsMore, NoMatch, PartialMatch, TextRange}
import scala.util.matching.Regex

trait Pattern[+Input] {

    def &[T >: Input](other: Pattern[T]): Pattern[T] = CompositePattern(this, other)

    def -->(action: => Unit): PatternCase[Input] = PatternCase(this, () => action)

    def tryMatch(input: Iterable[_ <: Input]): MatchResult

}

case class CompositePattern[+Input](pattern1: Pattern[Input], pattern2: Pattern[Input]) extends Pattern[Input] {
    override def &[T >: Input](other: Pattern[T]): Pattern[T] = CompositePattern(this, other)

    override def tryMatch(input: Iterable[_ <: Input]): MatchResult = {
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

case class RegexPattern(regexStr: String) extends Pattern[Char] {
    val regex = new Regex(regexStr)
    override def tryMatch(input: Iterable[_ <: Char]): MatchResult = ???
}