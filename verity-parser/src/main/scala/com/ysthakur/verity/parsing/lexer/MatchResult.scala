package com.ysthakur.verity.parsing.lexer

import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps


/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class Match(
    matched: Iterable[Char],
    start: Int,
    end: Int
)

/**
  * Represents the result of a match
  */
sealed trait MatchResult {
  def >(other: MatchResult): Boolean
  def <(other: MatchResult): Boolean = other > this
  def <=(other: MatchResult): Boolean = other >= this
  def >=(other: MatchResult): Boolean = this > other || this == other
  def higherOf(other: MatchResult): MatchResult = if (this < other) other else this
}

object MatchResult {
  def higher(mr1: MatchResult, mr2: MatchResult): MatchResult =
    if (mr1 < mr2) mr2 else mr1
}

/**
  * Ran out of input before it could finish matching
  */
object NeedsMore extends MatchResult {
  def >(other: MatchResult): Boolean = other == NoMatch
}

/**
  * A part of the input was matched, but not the end
  *
  * @param matched What was matched
  */
case class PartialMatch(matched: Match) extends MatchResult {
  override def >(other: MatchResult): Boolean = 
    other == NoMatch || 
    other == NeedsMore ||
      other.isInstanceOf[PartialMatch] && 
      other.asInstanceOf[PartialMatch].matched.end > this.matched.end
}

/**
  * The entire input matched. Yay!
  */
case class FullMatch(matched: Match, couldMatchMore: Boolean)
    extends MatchResult {  
  def >(other: MatchResult): Boolean = 
    !(other.isInstanceOf[FullMatch] && 
    !this.couldMatchMore && 
    other.asInstanceOf[FullMatch].couldMatchMore)
}

/**
  * None of the input matched
  */
object NoMatch extends MatchResult {
  def >(other: MatchResult): Boolean = false
}
