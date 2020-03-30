package com.ysthakur.parsing.grammar

import scala.language.postfixOps

import com.ysthakur.util._

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern[Input] {
  type Self <: Pattern[Input]
  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  val isFixed: Boolean
  def tryMatch(input: Iterable[Input]): MatchResult

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Input](other: Pattern[T]): Pattern[Input] =
    new CompositePattern(List(this, other.asInstanceOf[Pattern[Input]]))

  def |[T <: Input](other: Pattern[T]): Pattern[Input] =
    FunctionPattern((input: Iterable[Input]) => {
      val m1 = this.tryMatch(input)
      m1 match {
        case FullMatch(_, _) | PartialMatch(_) => m1
        case _ =>
          val m2 = other.tryMatch(input.as[Iterable[T]])
          m2 match {
            case FullMatch(_, _) | PartialMatch(_) => m2
            case _                                 => m2
          }
      }
    })

  def -->[Helper](action: Helper => Unit): PatternCase[Input, Helper] =
    PatternCase(this.asInstanceOf[Pattern[Input]], action)

  def * : Pattern[Input] = RepeatPattern(this, eager = true)
  def *? : Pattern[Input] = RepeatPattern(this, eager = false)
}
