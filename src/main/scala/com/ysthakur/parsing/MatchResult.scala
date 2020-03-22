package com.ysthakur.parsing

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
case class PartialMatch(textRange: TextRange) extends MatchResult

/**
 * The entire input matched. Yay!
 */
case class FullMatch() extends MatchResult

/**
 * None of the input matched
 */
case class NoMatch() extends MatchResult
