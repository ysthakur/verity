package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.Option
import scala.collection.mutable

import Pattern._

type Trace = ListBuffer[NamedPattern[Node, Node]]

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern {
  type Input <: Node
  type AsNode <: Node
  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  def isFixed: Boolean
  def isEager: Boolean
  //var superPattern: PatternClass[?]|Null = null
  
  def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult
  def create(matched: ParseResult): this.AsNode = ???

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: T): this.type - T =
    new ConsPattern[this.type, T](this, other)

  def |[T <: Pattern](other: T): OrPattern[this.type, T] =
    OrPattern(this, other)

  def * : Pattern  = RepeatPattern(this, isEager = true)
  def *? : Pattern = RepeatPattern(this, isEager = false)
  def ? : Pattern = MaybePattern(this)

//  def Extends(superPattern: PatternClass[?]): this.type = {
//    this.superPattern = superPattern
//    this
//  }

  //def doesExtend(superPattern: PatternClass[?]): Boolean = this.superPattern == superPattern
  //def subOf(other: Pattern): Boolean = this.superPattern == other
  def ==(other: Pattern): Boolean = ???

  /**
    * Print this when it doesn't match
    * @return
    */
  def expected(prevRes: ParseResult): List[String] // = List.empty
  def headOrEmpty[T](it: Iterable[T]): Any = if (it.isEmpty) "nothing" else it.head
}

trait MultiPattern extends Pattern {
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
  def pattern: Pattern = Pattern.allPatterns.getOrElse(name, throw new Error(s"Couldn't find pattern $name in allPatterns=${Pattern.allPatterns}"))
  override def isFixed: Boolean = pattern.isFixed
  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    try {
      val p = pattern
      p.tryMatch(input, offset, trace :+ this)
    } catch {
      case e: Throwable => throw new Error(s"Exception in pattern $name", e)
    }
  }

  override def expected(prevRes: ParseResult): List[String] = pattern.expected(prevRes)
  
  override def canEqual(that: Any): Boolean = that.isInstanceOf[NamedPattern[?, ?]]
  override def equals(obj: Any): Boolean = obj match {
    case NamedPattern(this.name) => true
    case _ => false
  }
  override def hashCode: Int = name.hashCode
}

object - {
  def unapply[A <: Node, B <: Node](arg: ConsNode[A, B]): Option[(A, B)] = {
    Some(arg.n1, arg.n2)
  }
}

object || {
  def unapply[A <: Node, B <: Node](arg: OrNode[A, B]): Option[(Node | Null, Node | Null)] = {
    arg match {
      case LeftNode(left) => Some((left, null))
      case RightNode(right) => Some((null, right))
    }
  }
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override type AsNode = ConsNode[p1.AsNode, p2.AsNode]
  
  override def isFixed: Boolean = p1.isFixed && p2.isFixed
  override def isEager: Boolean = p1.isEager && p2.isEager
  
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    val match1 = p1.tryMatch(input, offset, trace)
    match1 match {
      case Matched(create, rest, offset) => p2.tryMatch(rest, offset, trace) match {
        case Matched(create2, rest2, offset2) => 
          Matched(ConsNode(create(), create2()), rest2, offset2)
        case _ => Failed(headOrEmpty(rest), p2.expected(match1))
      }
      case _ => {
        val head = headOrEmpty(input)
        Failed(head, p1.expected(Failed(head, List.empty)))
      }
    }
  }

  override def expected(prevRes: ParseResult): List[String] = prevRes match {
    case f: Failed => p1.expected(prevRes)
    case m: Matched[?, ?] => p2.expected(Failed(m.rest, List.empty))
  }
}

case class OrPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override type Input = p1.Input | p2.Input
  override type AsNode = p1.AsNode | p2.AsNode

  override def isFixed: Boolean = p1.isFixed && p2.isFixed
  override def isEager: Boolean = p1.isEager || p2.isEager
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    p1.tryMatch(input, offset, trace) match {
      case Failed(_) => p2.tryMatch(input, offset, trace)
      case m => m
    }
  }
  override def expected(prevRes: ParseResult): List[String] = 
    p1.expected(prevRes) ++ p2.expected(prevRes)
}

case class MaybePattern(pattern: Pattern) extends Pattern {
  override val isEager: Boolean = false
  override val isFixed: Boolean = false
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult =
    pattern.tryMatch(input, offset, trace)
        .orElse(new Matched(() => null, input, offset, true))
  override def expected(prevRes: ParseResult): List[String] = "nothing" :: pattern.expected(prevRes)
}

/**
  *
  */
case class RepeatPattern(
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
  
  override final def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    tryMatch(input, offset, trace, 0, Matched.empty(input, Some(this)))
  }

  private def tryMatch(input: List[Node], offset: Int, trace: Trace, depth: Int, prevRes: ParseResult): ParseResult = {
    pattern.tryMatch(input, offset, trace) match {
      case failed: Failed => Matched.empty(input, Some(this))
      case res@Matched(_, rest, offset2) => res + tryMatch(rest, offset2, trace, depth, res)
    }
  }
  
  override def expected(prevRes: ParseResult): List[String] = pattern.expected(prevRes)
}
//
///**
// * A pattern that matches any one of a group of patterns
// */
//class PatternClass[N <: Node](val patterns: Pattern*) extends MultiPattern {
//
//  for (pattern <- patterns) pattern Extends this
//
//  override type AsNode = N
//
//  override lazy val isFixed: Boolean = patterns.forall(_.isFixed)
//  override lazy val isEager: Boolean = patterns.exists(_.isEager)
//
//  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult =
//    patterns.view.map(_.tryMatch(input, offset, trace)).find{_.isInstanceOf[Matched[?, ?]]}.getOrElse(Failed(List.empty))
//}