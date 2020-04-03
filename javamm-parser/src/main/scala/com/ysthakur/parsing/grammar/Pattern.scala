package com.ysthakur.parsing.grammar

import com.ysthakur.parsing.grammar.GrammarUtils._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern[Input] {
  type Self <: Pattern[Input]
  type I = Input

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  val isFixed: Boolean
  val isEager: Boolean
  def tryMatch(input: Iterable[Input], offset: Int): MatchResult

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern[_]](other: T): Self - T =
    new ConsPattern[Self, T](this.asInstanceOf[Self], other)

  def |[T <: Input](other: Pattern[T]): Pattern[Input] =
    FunctionPattern((input: Iterable[Input]) => {
      val m1 = this.tryMatch(input)
      m1 match {
        case FullMatch(_, _) | PartialMatch(_) => m1
        case _ =>
          val m2 = other.tryMatch(input.asInstanceOf[Iterable[T]])
          m2 match {
            case FullMatch(_, _) | PartialMatch(_) => m2
            case _                                 => m2
          }
      }
    })

  def -->[Helper](action: Helper => Unit): PatternCase[Input, Helper] =
    PatternCase(this.asInstanceOf[Pattern[Input]], action)

  def * : Pattern[Input]  = RepeatPattern(this, isEager = true)
  def *? : Pattern[Input] = RepeatPattern(this, isEager = false)
}

case class ConsPattern[T1 <: Pattern[_], T2 <: Pattern[_]](p1: T1, p2: T2)
    extends Pattern[p1.I with p2.I] {
  override type Self = com.ysthakur.parsing.grammar.-[T1, T2]
  override type I    = p1.I with p2.I
  override val isFixed: Boolean = p1.isFixed && p2.isFixed

  override def tryMatch(input: Iterable[I], offset: Int): MatchResult = {
    val match1 = p1.tryMatch(input)
    match1 match {
      case full: FullMatch[I] => full
      case part: PartialMatch[I] =>
        p2.tryMatch(input.slice(part.matched.end, input.size)) match {
          case FullMatch(matched2, cmm2) =>
            FullMatch(CompositeMatch(List(part.matched, matched2)), cmm2)
          case PartialMatch(matched2) =>
            PartialMatch(CompositeMatch(List(part.matched, matched2)))
          case m2 => m2
        }
      case _ => MatchResult.higher(match1, p2.tryMatch(input))
    }
  }
}

case class OrPattern[T1 <: Pattern[_], T2 <: Pattern[_]](p1: T1, p2: T2)
    extends Pattern[p1.I Either p2.I] {
  override type Self = T1 | T2
  override type I    = p1.I Either p2.I
  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override def tryMatch(input: Iterable[I]): MatchResult = {
    val input1 =
      input.takeWhile(_.isInstanceOf[p1.I]).asInstanceOf[Iterable[p1.I]]
    var match1: MatchResult = null
    var slice2 = if (input1.nonEmpty) {
      p1.tryMatch(input1) match {
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else match1 = PartialMatch(matched)
        case x => match1 = x
      }
      input.slice(input1.size, input.size)
    } else input

    val input2 =
      slice2.takeWhile(_.isInstanceOf[p2.I]).asInstanceOf[Iterable[p2.I]]
    var match2: MatchResult = null
    p2.tryMatch(input2) match {
      case full @ FullMatch(matched, couldMatchMore) =>
        if (input1.size == input.size) return full
        else match2 = full
      case x => match2 = x
    }
    return if (match1 >= match2) match1 else match2
  }
}

/**
  *
  * @param isEager Does not really matter to this pattern, but does matter
  *              to CompositePatterns
  */
case class RepeatPattern[Input](
    pattern: Pattern[Input],
    override val isEager: Boolean
) extends Pattern[Input] {

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false

  override def tryMatch(input: Iterable[Input]): MatchResult = {
    val matches = ListBuffer[Match[Input]]()
    ???
  }
}
