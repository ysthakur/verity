package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.Option

import Pattern._

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern {
  type Input <: Node
  type AsNode <: Node
  type MatchIn <: Match[_]
  type Trace = ListBuffer[Pattern]

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  val isFixed: Boolean
  val isEager: Boolean
  var superPattern: PatternClass[?]|Null = null

  // def copy: this.type
  def tryMatch(input: Iterable[Input], offset: Int, trace: Trace): MatchResult
  def create(matched: MatchIn): this.AsNode

  final def tryCreate(input: Iterable[this.Input], offset: Int, trace: Trace): (MatchResult, Option[this.AsNode]) = {
    val matchres = tryMatch(input, offset, trace)
    return (matchres, matchres match {
      case NeedsMore | NoManifest => None
      case PartialMatch(matched: MatchIn) => Some(create(matched : MatchIn))
      case FullMatch(matched: MatchIn, _) => Some(create(matched : MatchIn))
    })
  }

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: T): this.type - T =
    new ConsPattern[this.type, T](this.asInstanceOf[this.type], other)
//
//<<<<<<< Updated upstream
//  def |[T <: Pattern](other: Pattern): OrPattern[this.type, other.type] =
//=======
  def |[T <: Pattern](other: T): OrPattern[this.type, T] =
//>>>>>>> Stashed changes
    OrPattern(this, other)
  def &^[T <: Pattern](other: T): OptionPattern[this.type, T] =
    OptionPattern(this, other)

//   def -->[Helper](action: Helper => Unit): PatternCase[Input, Helper] =
//     PatternCase(this.asInstanceOf[Pattern[Input]], action)

  def * : Pattern  = RepeatPattern(this, isEager = true)
  def *? : Pattern = RepeatPattern(this, isEager = false)

  def Extends(superPattern: PatternClass[?]): this.type = {
    this.superPattern = superPattern
    this
  }

  def doesExtend(superPattern: PatternClass[?]): Boolean = this.superPattern == superPattern
  def subOf(other: Pattern): Boolean = this.superPattern == other
  def ==(other: Pattern): Boolean = ???
}

trait MultiPattern extends Pattern {
  protected val excluded: ListBuffer[Pattern] = ListBuffer(this)
  def patterns: Iterable[Pattern]
  for (pattern <- patterns) pattern match { case p: MultiPattern => p.excluded.addOne(this) }
}

object Pattern {
  type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  type *[T <: Pattern] = RepeatPattern
  type *?[T <: Pattern] = RepeatPattern

  val allPatterns: ListBuffer[Pattern] = ListBuffer()
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override type Input = p1.Input & p2.Input
  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override val isEager: Boolean = p1.isEager && p2.isEager

  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, Option[this.AsNode]) = ???
  override def tryMatch(input: Iterable[Input], offset: Int, trace: Trace): MatchResult = {
    val match1 = p1.tryMatch(input, offset, trace :+ this)
    match1 match {
      case full: FullMatch[Input] => full
      case part: PartialMatch[Input] =>
        p2.tryMatch(input.slice(part.matched.end, input.size), part.matched.end, trace :+ this) match {
          case FullMatch(matched2, cmm2) => 
            FullMatch(CompositeMatch(List(part.matched, matched2)), cmm2)
          case PartialMatch(matched2) =>
            PartialMatch(CompositeMatch(List(part.matched, matched2)))
          case m2 => m2
        }
      case _ => MatchResult.higher(match1, p2.tryMatch(input, offset, trace :+ this))
    }
  }

  override def create(matched: MatchIn): AsNode = {
    ???
  }

  override def patterns: Iterable[Pattern] = Array(p1, p2)
  //override def copy: ConsPattern[T1, T2] = ConsPattern(p1.copy, p2.copy)
}

case class OrPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override type Input = p1.Input | p2.Input
  override type AsNode = p1.AsNode | p2.AsNode

  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override val isEager: Boolean = p1.isEager || p2.isEager
  override def tryMatch(input: Iterable[this.Input], offset: Int, trace: Trace): MatchResult = {
    if (excluded.contains(p1)) 
      return p2.tryMatch(input.asInstanceOf[Iterable[p2.Input]], offset, trace :+ this)
    else if (excluded.contains(p2)) 
      return p1.tryMatch(input.asInstanceOf[Iterable[p1.Input]], offset, trace :+ this)

    val input1 = input.takeWhile(_.isInstanceOf[p1.Input]).asInstanceOf[Iterable[p1.Input]]
    var match1: MatchResult = NeedsMore
    var slice2 = if (input1.nonEmpty) {
      p1.tryMatch(input1, offset, trace :+ this) match {
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else match1 = PartialMatch(matched)
        case x => match1 = x
      }
      input.slice(input1.size, input.size)
    } else input

    val input2 =
      slice2.takeWhile(_.isInstanceOf[p2.Input]).asInstanceOf[Iterable[p2.Input]]
    var match2: MatchResult = 
      p2.tryMatch(input2, offset, trace :+ this) match {
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else full
        case x => x
      }
    return if (match1 >= match2) match1 else match2
  }
  override def create(res: MatchIn): AsNode = {
    val p = res.pattern
    p.create(res.asInstanceOf[p.MatchIn]).asInstanceOf[AsNode]
  }

  override def patterns: Iterable[Pattern] = Array(p1, p2)
  //override def copy: OrPattern[T1, T2] = OrPattern(p1.copy, p2.copy)
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

  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, Option[this.AsNode]) = ???
  override def tryMatch(input: Iterable[Input], offset: Int, trace: Trace): MatchResult = {
    val matches = ListBuffer[Match[Input]]()
    ???
  }

  override def create(matched: MatchIn): this.AsNode = {
    ???
  }
}

/**
 * A pattern that matches any one of a group of patterns
 */
class PatternClass[N <: Node](patterns_ : Pattern*) extends MultiPattern {

  val patterns = patterns_.filter(!excluded.contains(_))
  for (pattern <- patterns) pattern Extends this

  override type AsNode = N

  override val isFixed: Boolean = patterns.forall(_.isFixed)
  override val isEager: Boolean = patterns.exists(_.isEager)

  override def create(matched: MatchIn): AsNode = {
    val p = matched.pattern
    p.create(matched.asInstanceOf[p.MatchIn]).asInstanceOf[AsNode]
  }

  override def tryMatch(input: Iterable[Input], offset: Int, trace: Trace): MatchResult =
    patterns.foldRight(NoMatch : MatchResult) { (p, matchres) => 
      (if (trace.contains(p)) NoMatch 
      else p.tryMatch(
        input.asInstanceOf[Iterable[p.Input]],
        offset, 
        trace :+ this)
      ).higherOf(matchres)
    }

  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, Option[this.AsNode]) = ???
}

case class OptionPattern[T1 <: Pattern, T2 <: Pattern](wanted: T1, unwanted: T2) 
extends Pattern {
  override type AsNode = wanted.AsNode
  override type Input = wanted.Input
  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false
  override val isEager: Boolean = wanted.isEager

  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, Option[this.AsNode]) = ???

  override def tryMatch(input: Iterable[Input], offset: Int, trace: Trace): MatchResult = {
    val matchres = wanted.tryMatch(input, offset, trace :+ this)
    matchres match {
      case FullMatch(matched, couldMatchMore) => throw new Error()
      case PartialMatch(matched) => throw new Error()
      case _ => return matchres
    }
  }

  override def create(matched: MatchIn): this.AsNode = {
    ???
  }
}
