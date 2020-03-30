package com.ysthakur.parsing.grammar

trait Match[-Input] {
  val start: Int
  val end: Int
}

/**
  * A match made up of multiple little matches inside
  *
  * @param matches The matches this is made up of. Must be ordered.
  * @tparam Input
  */
class CompositeMatch[Input](
    val matches: Iterable[(Pattern[Input], Match[Input])])
    extends Match[Input] {
  override val start: Int = matches.head._2.start
  override val end: Int = matches.last._2.end

  def get(pattern: Pattern[Input]): Option[Match[Input]] = {
    matches.find(_._1 == pattern) match {
      case Some((_, m)) => Some(m)
      case None         => None
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case m: CompositeMatch[Input] => this.matches == m.matches
      case _                        => false
    }
  }
}

case class PatternMatch[Input](pattern: Pattern[Input],
                               matched: Iterable[Input],
                               override val start: Int,
                               override val end: Int)
    extends Match[Input]

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class ExactMatch[Input](matched: Iterable[Input],
                             override val start: Int,
                             override val end: Int)
    extends Match[Input]

object ExactMatch {
  def apply[Input](matched: Iterable[Input], tr: TextRange): ExactMatch[Input] =
    new ExactMatch(matched, tr.start, tr.end)
}

case class SingleMatch[Input](matched: Input, override val start: Int)
    extends Match[Input] {
  override val end: Int = start + 1
}

object EmptyMatch extends Match[Nothing] {
  override val start, end = 0
}
