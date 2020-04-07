package com.ysthakur.parsing

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.parser._
import com.ysthakur.util.as
import com.ysthakur.util.utils

import scala.language.implicitConversions

trait Match[+Input] {
  //type I <: Input
  def start: Int
  def end: Int
  def matched: Iterable[Input]
}

// object Match {
//   implicit def makePatternMatch[Input](
//       tuple: (Pattern[Input], Match[Input])
//   ): PatternMatch[Input] =
//     new PatternMatch(tuple._1, tuple._2.matched, tuple._2.start, tuple._2.end)
//   /*implicit def makeConsMatch[Input](
//       cons: List[Match[Input]]
//   ): CompositeMatch[Input] = CompositeMatch(cons)*/
// }

class PatternMatch[Input](
    pattern: Pattern,
    override val matched: Iterable[Input],
    override val start: Int,
    override val end: Int
) extends Match[Input] {
  override def equals(obj: Any): Boolean = {
    try {
      return obj.asInstanceOf[PatternMatch[Input]].unapply() == this.unapply()
    } catch {
      case e => false
    }
  }
  def unapply(): (Pattern, Iterable[Input], Int, Int) =
    (pattern, matched, start, end)
}

object PatternMatch {
  def apply[Input](
      pattern: Pattern,
      matched: Iterable[Input],
      start: Int,
      end: Int
  ): PatternMatch[Input] =
    new PatternMatch(pattern, matched, start, end)
}

// case class PatternWithMatch[P <: Pattern, M <: Match[_]]
//     (pattern: P, mach: M) 
//     extends Match[pattern.I] {

// } 

// case class RepeatMatch[Input](unit: Iterable[Input], numTimes: Int) extends Match[Input] {
  
// }

case class CompositeMatch[+Input](matches: List[Match[Input]]) extends Match[Input] {
  override val start: Int               = matches.head.start
  override val end: Int                 = matches.last.end
  override def matched: Iterable[Input] = matches.flatMap(_.matched)
}

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class ExactMatch[Input](
    override val matched: Iterable[Input],
    override val start: Int,
    override val end: Int
) extends Match[Input]

object ExactMatch {
  def apply[Input](matched: Iterable[Input], tr: TextRange): ExactMatch[Input] =
    new ExactMatch(matched, tr.start, tr.end)
}

case class RegexMatch(matched: Iterable[Char], override val start: Int, override val end: Int)
extends Match[Char] {
}

case class SingleMatch[Input <: Node](matchedPiece: Input, override val start: Int)
    extends Match[Input] {
  override val end: Int = start + 1
  override def matched  = Iterable(matchedPiece)
}

object EmptyMatch extends Match[Nothing] {
  override val start, end = 0
  override def matched: Iterable[Nothing] = Iterable.empty
}