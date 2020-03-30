package com.ysthakur.parsing.grammar

trait Match[type +Input] {
  type I = Input

  def start: Int
  def end: Int
  def matched: Iterable[Input]
}

object Match {
  implicit def makePatternMatch[Input](
      tuple: (Pattern[Input], Match[Input])
  ): PatternMatch[Input] =
    new PatternMatch(tuple._1, tuple._2.matched, tuple._2.start, tuple._2.end)
  implicit def makeConsMatch[Input](
      cons: List[Match[Input]]
  ): CompositeMatch[Input] = CompositeMatch(cons)
}

class PatternMatch[Input](
    pattern: Pattern[Input],
    override val matched: Iterable[Input],
    override val start: Int,
    override val end: Int
) extends Match[Input] {
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[PatternMatch[Input]] &&
    obj.asInstanceOf[PatternMatch[Input]].unapply() == this.unapply()
  }

  def unapply(): (Pattern[Input], Iterable[Input], Int, Int) =
    (pattern, matched, start, end)
}

object PatternMatch {
  def apply[Input](
      pattern: Pattern[Input],
      matched: Iterable[Input],
      start: Int,
      end: Int
  ): PatternMatch[Input] =
    new PatternMatch(pattern, matched, start, end)
}

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

case class SingleMatch[Input](matchedPiece: Input, override val start: Int)
    extends Match[Input] {
  override val end: Int = start + 1
  override def matched  = Iterable(matchedPiece)
}

object EmptyMatch extends Match[Nothing] {
  override val start, end = 0
  override def matched: Iterable[Nothing] = Iterable.empty
}
