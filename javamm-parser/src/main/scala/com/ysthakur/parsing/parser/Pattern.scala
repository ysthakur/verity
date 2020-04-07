package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Types._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

import PatternUtils._

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern {
  type Input <: Node
  type AsNode <: Node
  type MatchIn <: Match[_]

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  val isFixed: Boolean
  val isEager: Boolean
  def tryMatch(input: Iterable[Input], offset: Int): MatchResult = ???
  def create(matched: MatchIn): this.AsNode = ???

  def tryCreate(input: Iterable[this.Input], offset: Int): Either[MatchResult, this.AsNode]

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: T): this.type - T =
    new ConsPattern[this.type, T](this.asInstanceOf[this.type], other)

  def |[T <: Pattern](other: Pattern): OrPattern[this.type, other.type] =
    OrPattern(this, other)

//   def -->[Helper](action: Helper => Unit): PatternCase[Input, Helper] =
//     PatternCase(this.asInstanceOf[Pattern[Input]], action)

  def * : Pattern  = RepeatPattern(this, isEager = true)
  def *? : Pattern = RepeatPattern(this, isEager = false)
}

object PatternUtils {
  type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  type *[T <: Pattern] = RepeatPattern
  type *?[T <: Pattern] = RepeatPattern
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends Pattern {
  override type Input = p1.Input & p2.Input
  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override val isEager: Boolean = p1.isEager && p2.isEager

  override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
  override def tryMatch(input: Iterable[Input], offset: Int): MatchResult = {
    val match1 = p1.tryMatch(input, offset)
    match1 match {
      case full: FullMatch[Input] => full
      case part: PartialMatch[Input] =>
        p2.tryMatch(input.slice(part.matched.end, input.size), part.matched.end) match {
          case FullMatch(matched2, cmm2) =>
            FullMatch(CompositeMatch(List(part.matched, matched2)), cmm2)
          case PartialMatch(matched2) =>
            PartialMatch(CompositeMatch(List(part.matched, matched2)))
          case m2 => m2
        }
      case _ => MatchResult.higher(match1, p2.tryMatch(input, offset))
    }
  }

  override def create(matched: MatchIn): AsNode = {
    ???
  }
}

case class OrPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends Pattern {
  override type Input    = p1.Input | p2.Input
  override type AsNode = p1.AsNode | p2.AsNode

  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override val isEager: Boolean = p1.isEager || p2.isEager
  override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
  override def tryMatch(input: Iterable[this.Input], offset: Int): MatchResult = {
    val input1 =
      input.takeWhile(_.isInstanceOf[p1.Input]).asInstanceOf[Iterable[p1.Input]]
    var match1: MatchResult = null
    var slice2 = if (input1.nonEmpty) {
      p1.tryMatch(input1, offset) match {
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else match1 = PartialMatch(matched)
        case x => match1 = x
      }
      input.slice(input1.size, input.size)
    } else input

    val input2 =
      slice2.takeWhile(_.isInstanceOf[p2.Input]).asInstanceOf[Iterable[p2.Input]]
    var match2: MatchResult = null
    p2.tryMatch(input2, offset) match {
      case full @ FullMatch(matched, couldMatchMore) =>
        if (input1.size == input.size) return full
        else match2 = full
      case x => match2 = x
    }
    return if (match1 >= match2) match1 else match2
  }
  override def create(res: MatchIn): this.AsNode = {
    ???
  }
}

/**
  *
  */
case class RepeatPattern (
    pattern: Pattern,
    override val isEager: Boolean
) extends Pattern {

  override type AsNode = NodeList[pattern.AsNode]

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false

  override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
  override def tryMatch(input: Iterable[Input], offset: Int): MatchResult = {
    val matches = ListBuffer[Match[Input]]()
    ???
  }

  override def create(matched: MatchIn): this.AsNode = {
    ???
  }
}
