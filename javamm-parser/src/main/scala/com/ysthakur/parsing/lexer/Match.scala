package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.TextRange
import com.ysthakur.parsing.parser._

import scala.language.implicitConversions

trait Match[+Input] {
  type I <: Input
  def start: Int
  def end: Int
  def matched: Iterable[Input]
  val textRange: TextRange = TextRange(start, end)
}

case class ConsMatch[I1 <: Match[_], I2 <: Match[_]](m1: I1, m2: I2)
    extends Match[? >: m1.I with m2.I] {
  override def start: Int = m1.start
  override def end: Int = m2.start
  override lazy val matched = m1.matched ++ m2.matched
}

trait OrMatch[LI, RI](held: Match[LI] | Match[RI])
    extends Match[LI | RI] {
  override def start: Int = held.start
  override def end: Int = held.end
}

case class LeftMatch[+L <: Match[_], +R <: Match[_]](held: L) extends OrMatch[held.I, Any](held) {
  override def matched: Iterable[held.I] = held.matched.asInstanceOf[Iterable[held.I]]
}
case class RightMatch[+L <: Match[_], +R <: Match[_]](held: R) extends OrMatch[Any, held.I](held) {
  override def matched: Iterable[held.I] = held.matched.asInstanceOf[Iterable[held.I]]
}

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class ExactMatch[Input] private (
    override val matched: Iterable[Input],
    override val start: Int,
    override val end: Int
) extends Match[Input] {
}

object ExactMatch {
  def apply[Input](matched: Iterable[Input], tr: TextRange): ExactMatch[Input] =
    new ExactMatch(matched, tr.start, tr.end)
}

case class RegexMatch(matched: Iterable[Char], override val start: Int, override val end: Int)
extends Match[Char]

case class SingleMatch[Input <: Node](matchedPiece: Input, override val start: Int)
    extends Match[Input] {
  override val end: Int = start + 1
  override def matched  = Iterable(matchedPiece)
}

object EmptyMatch extends Match[Nothing] {
  override val start, end = 0
  override val matched: Iterable[Nothing] = Iterable.empty
}