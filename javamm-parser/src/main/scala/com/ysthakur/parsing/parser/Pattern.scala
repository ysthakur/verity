package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.Option
import Pattern._

import scala.collection.mutable

type Trace = ListBuffer[NamedPattern[Node, Node]]

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
  def isFixed: Boolean
  def isEager: Boolean
  var superPattern: PatternClass[?]|Null = null
  
  // def copy: this.type
  def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult
  def create(matched: MatchIn): this.AsNode

  /*final def tryCreate(input: Iterable[this.Input], offset: Int, trace: Trace): (ParseResult, Option[this.AsNode]) = {
    val matchres = tryMatch(input, offset, trace)
    return (matchres, matchres match {
      case NeedsMore | NoManifest => None
      case PartialMatch(matched: MatchIn) => Some(create(matched : MatchIn))
      case FullMatch(matched: MatchIn, _) => Some(create(matched : MatchIn))
    })
  }*/

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: T): this.type - T =
    new ConsPattern[this.type, T](this.asInstanceOf[this.type], other)

  def |[T <: Pattern](other: T): OrPattern[this.type, T] =
    OrPattern(this, other)
  /*def &^[T <: Pattern](other: T): OptionPattern[this.type, T] =
    OptionPattern(this, other)*/

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

def (patternName: String) := (pattern: => Pattern): Unit = {
  Pattern.allPatterns.put(patternName, pattern)
}

object Pattern {
  type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  type *[T <: Pattern] = RepeatPattern
  type *?[T <: Pattern] = RepeatPattern

  val allPatterns: mutable.LinkedHashMap[String, Pattern] = mutable.LinkedHashMap()
}

implicit def toPattern(patternName: String): Pattern = NamedPattern(patternName)

case class NamedPattern[+I <: Node, +N <: Node](name: String) extends Pattern {
  override type AsNode <: N
  def pattern: Pattern = Pattern.allPatterns.getOrElse(name, throw new Error())
  override def isFixed: Boolean = pattern.isFixed
  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    val p = pattern
    p.tryMatch(input, offset, trace :+ this)
  }
  override def create(matched: MatchIn): AsNode = {
    val p = pattern
    p.create(matched.asInstanceOf[p.MatchIn]).asInstanceOf[AsNode]
  }
  override def canEqual(that: Any): Boolean = that.isInstanceOf[NamedPattern[?, ?]]
  override def equals(obj: Any): Boolean = obj match {
    case NamedPattern(name) => true
    case _ => false
  }
  override def hashCode: Int = name.hashCode
}

object - {
  def unapply[A <: Match[_], B <: Match[_]](arg: ConsMatch[A, B]): Option[(Node, Node)] = {
    Some(arg.m1.toNode, arg.m2.toNode)
  }
}

object || {
  def unapply[A <: Match[_], B <: Match[_]](arg: OrMatch[A, B]): Option[(Node | Null, Node | Null)] = {
    arg match {
      case left: LeftMatch[A, B] => Some(left.held.toNode, null)
      case right: RightMatch[A, B] => Some(null, right.held.toNode)
      case _ => None
    }
  }
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override type AsNode = ConsNode[p1.AsNode, p2.AsNode]
  
  override val isFixed: Boolean = p1.isFixed && p2.isFixed
  override val isEager: Boolean = p1.isEager && p2.isEager

  // override def tryCreate(input: Iterable[Node], offset: Int): (ParseResult, Option[this.AsNode]) = ???
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    if (trace.last == p1) {
      if (trace.last == p2) return Failed
      else return p2.tryMatch(input, offset, trace)
    }
    val match1 = p1.tryMatch(input, offset, trace)
    match1 match {
      case Matched(create, rest, offset) => p2.tryMatch(rest, offset, trace) match {
        case Matched(create2, rest2, offset2) => 
          Matched(ConsNode(create(), create2()), rest2, offset2)
        case _ => Failed
      }
      case _ => p2.tryMatch(input, offset, trace)
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
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    p1.tryMatch(input, offset, trace) match {
      case Failed => p2.tryMatch(input, offset, trace)
      case m => m
    }
    /*val input1 = input.takeWhile(_.isInstanceOf[p1.Input]).asInstanceOf[Iterable[p1.Input]]
    var match1: ParseResult = Failed
    var slice2 = if (input1.nonEmpty) {
      p1.tryMatch(input1, offset, trace) match {
        case m: Matched[?, ?] => return m
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else match1 = PartialMatch(matched)
        case x => match1 = x
      }
      input.slice(input1.size, input.size)
    } else input

    val input2 =
      slice2.takeWhile(_.isInstanceOf[p2.Input]).asInstanceOf[Iterable[p2.Input]]
    var match2: ParseResult = 
      p2.tryMatch(input2, offset, trace) match {
        case full @ FullMatch(matched, couldMatchMore) =>
          if (input1.size == input.size) return full
          else full
        case x => x
      }
    return if (match1 >= match2) match1 else match2*/
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

  // override def tryCreate(input: Iterable[Node], offset: Int): (ParseResult, Option[this.AsNode]) = ???
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
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

  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    patterns.foreach { _.tryMatch(input, offset, trace) match {
        case matched: Matched[?, ?] => return matched
      }
    }
    return Failed
  }

  // override def tryCreate(input: Iterable[Node], offset: Int): (ParseResult, Option[this.AsNode]) = ???
}

/*
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

  // override def tryCreate(input: Iterable[Node], offset: Int): (ParseResult, Option[this.AsNode]) = ???

  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    val matchres = wanted.tryMatch(input, offset, trace)
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
*/
