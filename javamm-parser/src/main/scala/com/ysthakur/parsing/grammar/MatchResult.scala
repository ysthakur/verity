package com.ysthakur.parsing.grammar

import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps

import com.ysthakur.parsing.Match

/**
  * Represents a range of text (or anything else, really).
  *
  * @param start The start of this range (inclusive)
  * @param end   The end of this range (exclusive)
  */
case class TextRange(start: Int, end: Int)

/**
  * Represents the result of a match
  */
sealed trait MatchResult {
  def >(other: MatchResult): Boolean
  def <(other: MatchResult): Boolean = other > this
  def <=(other: MatchResult): Boolean = other >= this
  def >=(other: MatchResult): Boolean = this > other || this == other
  def higher(other: MatchResult): MatchResult = if (this < other) other else this
}

object MatchResult {
  def higher(mr1: MatchResult, mr2: MatchResult): MatchResult =
    if (mr1 < mr2) mr2 else mr1
}

/**
  * Ran out of input before it could finish matching
  */
case class NeedsMore() extends MatchResult {
  def >(other: MatchResult): Boolean = other.isInstanceOf[NoMatch]
}

/**
  * A part of the input was matched, but not the end
  *
  * @param matched What was matched
  */
case class PartialMatch[Input](matched: Match[Input]) extends MatchResult {
  def unapply(): (Int, Int) = (matched.start, matched.end)
  override def >(other: MatchResult): Boolean = 
    other.isInstanceOf[NoMatch] || 
    other.isInstanceOf[NeedsMore] ||
      other.isInstanceOf[PartialMatch[Input]] && 
      other.asInstanceOf[PartialMatch[Input]].matched.end > this.matched.end
}

/**
  * The entire input matched. Yay!
  */
case class FullMatch[Input](matched: Match[Input], couldMatchMore: Boolean)
    extends MatchResult {  
  def >(other: MatchResult): Boolean = 
    !(other.isInstanceOf[FullMatch[Input]] && 
    !this.couldMatchMore && 
    other.asInstanceOf[FullMatch[Input]].couldMatchMore)
}

/**
  * None of the input matched
  */
case class NoMatch() extends MatchResult {
  def >(other: MatchResult): Boolean = false
}
