package com.ysthakur.parsing.grammar

case class RepeatPattern[Input](pattern: Pattern[Input], eager: Boolean)
    extends Pattern[Input] {

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false

  override def tryMatch(input: Iterable[Input]): MatchResult = ???
}
