package com.ysthakur.parsing

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.TextRange
import com.ysthakur.parsing.parser._
import com.ysthakur.util.{as, utils}

import scala.language.implicitConversions

trait Match[+Input] {
  type I <: Input
  def start: Int
  def end: Int
  def matched: Iterable[Input]
  val textRange: TextRange = TextRange(start, end)
  def pattern: Pattern //= ???
  def toNode: Node = {
    val p = pattern
    p.create(this.asInstanceOf[p.MatchIn])
  }
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
    override val pattern: Pattern,
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

case class ConsMatch[I1 <: Match[_], I2 <: Match[_]](m1: I1, m2: I2)
    extends Match[? >: m1.I with m2.I] {
  override def start: Int = m1.start
  override def end: Int = m2.start
  override lazy val matched = m1.matched ++ m2.matched
  override def pattern: Pattern = ???
}

trait OrMatch[LI, RI](held: Match[LI] | Match[RI])
    extends Match[LI | RI] {
  override def start: Int = held.start
  override def end: Int = held.end
  def get: Match[LI] | Match[RI]
  override def pattern: Pattern = held.pattern
  override def toNode: Node = try {
    val p = pattern
    p.create(held.asInstanceOf[p.MatchIn])
  } catch {
    case e: Throwable => super.toNode
  }
}

case class LeftMatch[+L <: Match[_], +R <: Match[_]](held: L) extends OrMatch[held.I, Any](held) {
  override def matched: Iterable[held.I] = held.matched.asInstanceOf[Iterable[held.I]]
  override def get: L = held
}
case class RightMatch[+L <: Match[_], +R <: Match[_]](held: R) extends OrMatch[Any, held.I](held) {
  override def matched: Iterable[held.I] = held.matched.asInstanceOf[Iterable[held.I]]
  override def get: R = held
}

// case class PatternWithMatch[P <: Pattern, M <: Match[_]]
//     (pattern: P, mach: M) 
//     extends Match[pattern.I] {

// } 

// case class RepeatMatch[Input](unit: Iterable[Input], numTimes: Int) extends Match[Input] {
  
// }
/*
case class CompositeMatch[+Input](matches: List[Match[Input]]) extends Match[Input] {
  override val start: Int               = matches.head.start
  override val end: Int                 = matches.last.end
  override def matched: Iterable[Input] = matches.flatMap(_.matched)
}*/

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class ExactMatch[Input] private (
    override val matched: Iterable[Input],
    override val start: Int,
    override val end: Int,
    override val textRange: TextRange
) extends Match[Input] {
  override def toNode: Node = try {
    val p = pattern
    p.create(this.asInstanceOf[p.MatchIn])
  } catch {
    case e: Throwable => super.toNode
  }
  override def pattern: Pattern = ???
}

object ExactMatch {
  def apply[Input](matched: Iterable[Input], tr: TextRange): ExactMatch[Input] =
    new ExactMatch(matched, tr.start, tr.end, tr)
}

case class RegexMatch(matched: Iterable[Char], override val start: Int, override val end: Int)
extends Match[Char] {
  override def pattern: Pattern = ???
}

case class SingleMatch[Input <: Node](matchedPiece: Input, override val start: Int)
    extends Match[Input] {
  override val end: Int = start + 1
  override def matched  = Iterable(matchedPiece)
  override def toNode: Node = try {
    val p = pattern
    p.create(matchedPiece.asInstanceOf[p.MatchIn])
  } catch {
    case e: Throwable => super.toNode
  }
  override def pattern: Pattern = ???
}

object EmptyMatch extends Match[Nothing] {
  override val start, end = 0
  override val matched: Iterable[Nothing] = Iterable.empty
  override val pattern: Pattern = ???
}