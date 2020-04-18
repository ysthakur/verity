package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.Types._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.Option
import scala.collection.mutable
import Pattern._
import com.ysthakur.parsing.lexer.{EmptyToken, Position, Tok, Token, TokenType}

type Trace = ListBuffer[NamedPattern[Node, TextNode]]

/**
  * A pattern, like regex, that matches input
  *
  * @tparam Input The type of the input (Iterable of Char or Token)
  */
trait Pattern {
  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  def isFixed: Boolean
  def isEager: Boolean
  //var superPattern: PatternClass[?]|Null = null
  
  def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult

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
  def headOrEmpty(it: Iterable[Token[?]]): Token[?] = if (it.isEmpty) EmptyToken else it.head
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

case class NamedPattern[+I <: Node, +N <: TextNode](name: String) extends Pattern {
  def pattern: Pattern = 
    Pattern.allPatterns.getOrElse(name, 
      throw new Error(s"Couldn't find pattern $name in allPatterns=${Pattern.allPatterns}"))
  override def isFixed: Boolean = pattern.isFixed
  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult = {
    try {
      val p = pattern
      p.tryMatch(input, offset, trace :+ this)
    } catch {
      case e: Throwable => throw new Error(s"Exception in pattern $name", e)
    }
  }
  
  override def canEqual(that: Any): Boolean = that.isInstanceOf[NamedPattern[?, ?]]
  override def equals(obj: Any): Boolean = obj match {
    case NamedPattern(this.name) => true
    case _ => false
  }
  override def hashCode: Int = name.hashCode
}

object - {
  def unapply[A <: TextNode, B <: TextNode](arg: ConsNode[A, B]): Option[(A, B)] = {
    Some(arg.n1, arg.n2)
  }
}

object || {
  def unapply[A <: TextNode, B <: TextNode](arg: OrNode[A, B]): Option[(TextNode | Null, TextNode | Null)] = {
    arg match {
      case LeftNode(left) => Some((left, null))
      case RightNode(right) => Some((null, right))
    }
  }
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  
  override def isFixed: Boolean = p1.isFixed && p2.isFixed
  override def isEager: Boolean = p1.isEager && p2.isEager
  
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult = {
    val match1 = p1.tryMatch(input, offset, trace)
    match1 match {
      case Matched(create, rest, offset) => p2.tryMatch(rest, offset, trace) match {
        case Matched(create2, rest2, offset2) => 
          Matched(() => ConsNode(create(), create2()), rest2, offset2)
        case failed => failed
      }
      case failed => failed
    }
  }
}

case class OrPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  override def isFixed: Boolean = p1.isFixed && p2.isFixed
  override def isEager: Boolean = p1.isEager || p2.isEager
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult = {
    p1.tryMatch(input, offset, trace) match {
      case Failed(_) => p2.tryMatch(input, offset, trace)
      case m => m
    }
  }
}

case class MaybePattern(pattern: Pattern) extends Pattern {
  override val isEager: Boolean = false
  override val isFixed: Boolean = false
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult =
    pattern.tryMatch(input, offset, trace)
        .orElse(new Matched(() => null, input, offset, true))
}

/**
  *
  */
case class RepeatPattern(
    pattern: Pattern,
    override val isEager: Boolean
) extends Pattern {

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false
  
  override final def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult = {
    val (nodes: ListBuffer[() => TextNode], rest: List[Tok], endOffset: Int) = 
      tryMatch(input, offset, trace, ListBuffer())
    Matched(() => NodeList(nodes.map(_())), rest, endOffset)
  }

  @scala.annotation.tailrec
  private def tryMatch(input: List[Tok],
                       offset: Int,
                       trace: Trace,
                       prev: ListBuffer[() => TextNode]): (ListBuffer[() => TextNode], List[Node], Int) = {
    pattern.tryMatch(input, offset, trace) match {
      case Matched(create, rest, offset2) => {
        prev.addOne(create)
        println(s"Matched! ${create()}")
        tryMatch(rest, offset2, trace, prev)
      }
      case f => {
        println(s"Didn't match $input $f")
        (prev, input, offset)
      }
    }
  }
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
//  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult =
//    patterns.view.map(_.tryMatch(input, offset, trace)).find{_.isInstanceOf[Matched[?, ?]]}.getOrElse(Failed(List.empty))
//}