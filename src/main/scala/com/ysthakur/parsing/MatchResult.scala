package com.ysthakur.parsing

/**
 * Represents a range of text (or anything else, really).
 * @param start The start of this range (inclusive)
 * @param end The end of this range (exclusive)
 */
case class TextRange(start: Int, end: Int)

/**
 * Represents the result of a match
 */
sealed trait MatchResult

/**
 * Ran out of input before it could finish matching
 */
case class NeedsMore() extends MatchResult

/**
 * A part of the input was matched, but not the end
 * @param textRange The range of text that was matched
 */
case class PartialMatch(textRange: TextRange) extends MatchResult {
    def apply(start: Int, end: Int): PartialMatch = PartialMatch(TextRange(start, end))
}

/**
 * The entire input matched. Yay!
 */
case class FullMatch(couldMatchMore: Boolean) extends MatchResult

/**
 * None of the input matched
 */
case class NoMatch() extends MatchResult
