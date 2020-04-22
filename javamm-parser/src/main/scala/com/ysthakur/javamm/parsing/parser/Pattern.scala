package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.CompilationError
import com.ysthakur.javamm.parsing.ast._
import com.ysthakur.javamm.parsing.ast.Types._
import com.ysthakur.javamm.parsing.{Position, TextRange}
import com.ysthakur.javamm.parsing.lexer.{EmptyToken, Tok}

import scala.collection.mutable.ListBuffer
import scala.Option
import scala.collection.mutable
import scala.util.control.Breaks._

type Trace = ListBuffer[INamedPattern]

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
//  def isFixed: Boolean
//  def isEager: Boolean
  var _superPattern: INamedPattern|Null = null
  def superPattern: INamedPattern|Null = _superPattern
  
  def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: T): ConsPattern[this.type, T] =
    new ConsPattern[this.type, T](this, other)

  def |[T <: Pattern](other: T): OrPattern[this.type, T] =
    OrPattern(this, other)

  def * : Pattern  = RepeatPattern(this/*, isEager = true*/)
  //def *? : Pattern = RepeatPattern(this/*, isEager = false*/)
  def ? : Pattern = MaybePattern(this)

  def Extends(superPattern: INamedPattern): this.type = {
    this._superPattern = superPattern
    this
  }

  //def doesExtend(superPattern: PatternClass): Boolean = this.superPattern == superPattern
  def subOf(other: Pattern): Boolean = other == this.superPattern
  def ==(other: Pattern): Boolean = ???
  def headOrEmpty(it: Iterable[Tok]): Tok = if (it.isEmpty) EmptyToken else it.head
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

implicit def toNamedPattern(patternName: String): INamedPattern = PatternRef(patternName)

trait INamedPattern extends Pattern {
  val name: String
  //override def canEqual(that: Any): Boolean = that.isInstanceOf[PatternRef]
  override def equals(obj: Any): Boolean = obj match {
    case named: INamedPattern => this.name == named.name
    case _ => false
  }
  override def ==(other: Pattern): Boolean = this.equals(other)
  override def hashCode: Int = name.hashCode
}

/**
  * A reference to another pattern, to allow patterns dependent
  * on each other (mostly for left-recursion)
  * @param name The name of the pattern referred to
  */
case class PatternRef(override val name: String) extends INamedPattern {
  lazy val pattern: Pattern = 
    Pattern.allPatterns.getOrElse(name, 
      throw new Error(s"Couldn't find pattern $name in allPatterns=${Pattern.allPatterns}"))
  def isLeftRecursive(trace: ListBuffer[INamedPattern]): Boolean = {
    /*if (trace.isEmpty || !trace.exists(_ == this)) return false
    if (trace.last == this) return true
//    val trace2 = trace.filter(p => this.subOf(p))
//    if (trace2.isEmpty) return false
//    if (!trace2.exists(_ == this)) return false
//    if (trace2.last == this) return true
    isLeftRecursive(trace.filter(p => !this.subOf(p)))*/
    if (!pattern.isInstanceOf[PatternClass] && trace.contains(this)) {
      println(s"Is left-recursive this=$name $trace")
      true
    } else false
   /* var res = false
    breakable {
      for (i <- (trace.size - 1 to 0) by -1) {
        val elem = trace.apply(i)
        if (elem == this) {
          res = true
          break
        }
        else if (!this.subOf(elem)) break
      }
    }
    return res*/
  }
  override def superPattern: INamedPattern = pattern.superPattern
//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    try {
      println(s"I am $name, Trace = $trace, input = ${headOrEmpty(input)}")
      val x = 9;
      //TODO fix this with a variable or something
      if (trace.size > 100) throw new CompilationError(s"Trace is too big (${trace.mkString(",")}")
//      if (trace.nonEmpty && trace.last == this) {
//        //println(s"Trace = $trace")
//        println("\t\tAnd I have failed")
//        return Failed(headOrEmpty(input), List("Not left recursion"), start)
//      }
      if (trace.nonEmpty && isLeftRecursive(trace)) {
        println("\tAnd I have failed")
        return Failed(headOrEmpty(input), List(), start)
      }
      val p = pattern
      val newTrace = if (trace.nonEmpty && trace.last.subOf(this)) trace else trace :+ this
      p.tryMatch(input, start, newTrace) match {
        case Matched(create, rest, range) => println(s"Matched $name! ${create()}");Matched(() => create() match {
          case orNode: OrNode[?, ?] => orNode //.flatten
          case other => other
        }, rest, range)
        case f => {
          println(s"\tPattern $pattern has failed!!! $f");f
        }
      }
    } catch {
      case e: Throwable => throw new Error(s"Exception in pattern $name", e)
    }
  }
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
  
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager && p2.isEager
  
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    val match1 = p1.tryMatch(input, start, trace)
    match1 match {
      case Matched(create, rest, range) => p2.tryMatch(rest, range.end, ListBuffer()) match {
        case Matched(create2, rest2, range2) => 
          Matched(() => ConsNode(create(), create2()), rest2, TextRange(start, range2.end))
        case failed => failed
      }
      case failed => failed
    }
  }
}

case class OrPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager || p2.isEager
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    try {
      p1.tryMatch(input, start, trace) match {
        case Failed(_) => p2.tryMatch(input, start, trace)
        case m => m
      }
    } catch {
      case e => throw new CompilationError(s"Exception on input ${headOrEmpty(input)}", e)
    }
  }
}

case class MaybePattern(pattern: Pattern) extends Pattern {
//  override val isEager: Boolean = false
//  override val isFixed: Boolean = false
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    pattern.tryMatch(input, start, trace)
        .orElse(new Matched(() => null, input, TextRange.empty(start), true))
}

/**
  *
  */
case class RepeatPattern(
    pattern: Pattern,
//    override val isEager: Boolean
) extends Pattern {

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
//  override val isFixed: Boolean = false
  
  override final def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    if (input.nonEmpty) {
      val (nodes: ListBuffer[() => Node], rest: List[Tok], startPos: Position, endPos: Position) =
        tryMatch(input, start, trace, ListBuffer())
      Matched(() => NodeList(nodes.map(_())), rest, TextRange(startPos, endPos))
    } else {
      new Matched(() => NodeList(ListBuffer()), input, TextRange.empty(start), true)
    }
  }

  @scala.annotation.tailrec
  private def tryMatch(input: List[Tok],
                       start: Position,
                       trace: Trace,
                       prev: ListBuffer[() => Node]
                      ): (ListBuffer[() => Node], List[Tok], Position, Position) = {
    pattern.tryMatch(input, start, trace) match {
      case Matched(create, rest, offset2) => {
        prev.addOne(create)
        //println(s"Matched! ${create()}")
        tryMatch(rest, start, trace, prev)
      }
      case f: Failed => {
        //println(s"Didn't match $input $f")
        (prev, input, start, f.pos)
      }
    }
  }
}

/**
 * A pattern that matches any one of a group of patterns
 */
class PatternClass(override val name: String, val patterns: Pattern*) 
    extends MultiPattern with INamedPattern {

//  override lazy val isFixed: Boolean = patterns.forall(_.isFixed)
//  override lazy val isEager: Boolean = patterns.exists(_.isEager)
  
  for (pattern <- patterns) pattern Extends this
  
  override def tryMatch(input: List[Tok], pos: Position, trace: Trace): ParseResult = {
    println(s"Trace = $trace")
    patterns.view
        .map(p => p.tryMatch(input, pos, {println(s"asdfasd$trace");trace}) match {
          case m: Matched[?, ?] => println(s"Matched $p!!");m
          case f => f
        })
        .find(_.isInstanceOf[Matched[?, ?]])
        .getOrElse(Failed(headOrEmpty(input), List.empty, pos))
  }
}

object PatternClass {
  def make(name: String, patterns: Pattern*): Unit = {
    Pattern.allPatterns.update(name, new PatternClass(name, patterns: _*))
  }
}


case class FunctionPattern(matchFun: (List[Tok], Position) => ParseResult,
                              //    override val isFixed: Boolean = false,
                              //    override val isEager: Boolean = true
                          ) extends Pattern {
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    matchFun(input, start)
  override def ==(other: Pattern): Boolean = this.equals(other)
}