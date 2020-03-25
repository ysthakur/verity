package com.ysthakur.parsing

import scala.language.existentials

/**
 *
 * @param pattern
 * @param action
 */
case class PatternCase[Input, Helper](pattern: Pattern[Input], action: Helper => Unit)

/**
 * A pattern, like regex, that matches input
 * @tparam Input The type of the input (Iterable of Char or Token)
 */
trait Pattern[Input] {

    /**
     * Whether or not it always matches the same input.
     * If false, it might be a valid identifier or something
     * that takes a variable length input or something like that
     */
    val isFixed: Boolean

    /**
     * Just to compose multiple patterns. Match this pattern first, then
     * pattern `other`. It's left associative
     * @param other
     * @return
     */
    def &(other: Pattern[Input]): Pattern[Input] = CompositePattern(this, other)

    def -->[Helper](action: Helper => Unit): PatternCase[Input, Helper] =
        PatternCase(this.asInstanceOf, action)

    def tryMatch[T <: Iterable[Input]](input: T): MatchResult

}

/**
 * Used to match first `pattern1`, then `pattern2`
 * @param pattern1 The first thing being matched
 * @param pattern2 The second thing being matched
 * @tparam Input
 */
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

    override val isFixed: Boolean = pattern1.isFixed && pattern2.isFixed
}