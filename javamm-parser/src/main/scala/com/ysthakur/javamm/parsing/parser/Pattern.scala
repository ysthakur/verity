package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.CompilationError
import com.ysthakur.javamm.parsing.ast.infile._
import com.ysthakur.javamm.parsing.ast._
import com.ysthakur.javamm.parsing.{Position, TextRange}
import com.ysthakur.javamm.parsing.lexer.{EmptyToken, Tok, Token}

import scala.collection.mutable.ListBuffer
import scala.Option
import scala.collection.mutable
import scala.util.control.Breaks._

type Trace = ListBuffer[(TextRange|Null, INamedPattern)]

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
  //def ==(other: Pattern): Boolean = ???
  def headOrEmpty(it: Iterable[Tok]): Tok = if (it.isEmpty) EmptyToken else it.head
}

trait MultiPattern extends Pattern {
}

def (patternName: String) := (pattern: => Pattern): Unit = {

import com.ysthakur.javamm.parsing.ast.infile.EmptyNode

Pattern.allPatterns.put(patternName, pattern)
}

object Pattern {
  //type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  //type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  //type *[T <: Pattern] = RepeatPattern
  //type *?[T <: Pattern] = RepeatPattern

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
  //override def ==(other: Pattern): Boolean = this.equals(other)
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
  def isLeftRecursive(trace: Trace, tr: TextRange): Boolean = {
    if (!pattern.isInstanceOf[PatternClass] && trace.exists(p => p._2 == this && p._1 == tr)) {
      //println(s"Is left-recursive this=$name $trace")
      true
    } else false
  }
  override def superPattern: INamedPattern = pattern.superPattern
//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    try {
      //println(s"I am $name, Trace = $trace, input = ${headOrEmpty(input)}")
      val x = 9;
      //TODO fix this with a variable or something
      if (trace.size > 100) throw new CompilationError(s"Trace is too big (${trace.mkString(",")}")
      if (trace.nonEmpty && isLeftRecursive(trace, textRangeToEnd(start, input))) {
        //println("\tAnd I have failed")
        return Failed(headOrEmpty(input), List(), start)
      }
      val p = pattern
      val newTrace = if (trace.nonEmpty && trace.last._2.subOf(this)) trace else trace :+ (textRangeToEnd(start, input), this)
      p.tryMatch(input, start, newTrace) match {
        case Matched(create, rest, range) => /*println(s"Matched $name! ${create()}");*/Matched(() => create() /*match {
          case orNode: OrNode[?, ?] => orNode //.flatten
          case other => other
        }*/, rest, range)
        case f => {
          //println(s"\tPattern $pattern has failed!!! $f");
          f
        }
      }
    } catch {
      case e: Throwable => throw new Error(s"Exception in pattern $name", e)
    }
  }
}

def textRangeToEnd(start: Position, it: Iterable[Token[_]]): TextRange = TextRange(start, it.last.range.end)

object - {
  def unapply[A <: Node, B <: Node](arg: ConsNode[A, B]): Option[(A|EmptyNode.type, B|EmptyNode.type)] = {
    Some(arg.n1, arg.n2)
  }
}

object || {
  def unapply[A <: Node, B <: Node](arg: OrNode[A, B]): Option[(Node, Node)] = {
    arg match {
      case LeftNode(left) => Some((left, EmptyNode))
      case RightNode(right) => Some((EmptyNode, right))
    }
  }
}

case class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, p2: T2)
    extends MultiPattern {
  
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager && p2.isEager
  
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    p2 match {
      case pattern: TokenTypePattern =>
        val firstPart = input.takeWhile(_.tokenType != pattern.tokenType)
        if (firstPart.size == input.size) return Failed(headOrEmpty(input), List(), start)
        else {
          val token = input(firstPart.size)
          p1.tryMatch(input, start, trace) match {
            case Matched(create, rest, range) => 
              if (rest.nonEmpty) {
                if (rest.head == token) {
                  Matched(() => ConsNode(create(), token), rest.tail, TextRange(start, token.range.end))
                } else Failed(rest.head, List(token.tokenType.toString), range.end)
              } else {
                Failed(rest.head, List(token.tokenType.toString), range.end)
              }
            case f => f
          }
        }
      case _ =>
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
        .orElse(new Matched(() => EmptyNode, input, TextRange.empty(start), true))
}

/**
 * @param p1 The first part (an expression)
 * @param p2 The second part (some binary operator and another expression)
 */
case class LeftAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    p1.tryMatch(input, start, trace) match {
      case m@Matched(create1, rest1, tr1) => 
        println(s"Matched first, create1=${create1()}")
        p2.tryMatch(rest1, tr1.end, trace) match {
          case m2@Matched(create2, rest2, tr2) => 
            println(s"Iin tryMatch, create2=${create2()}")
            keepMatching(Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end)))
          case f => 
            println("Failed on second")
            f
        }
      case f => 
        println(s"Failed p1=$p1, boohoo")
        f
    }

  def keepMatching(lastRes: Matched[_, _]): ParseResult = {
    val Matched(create1, rest1, tr1) = lastRes
    p2.tryMatch(rest1, tr1.end, ListBuffer()) match {
      case m2@Matched(create2, rest2, tr2) => 
        println(s"Matched m2, create2=${create2()}")
        keepMatching(Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end)))
      case f => 
        println(s"Failed, lastRes=${lastRes.create()}")
        lastRes
    }
  }
}

/**
 * @param p1 The first part, e.g., an expression and some binary operator
 * @param p2 The second part, e.g., expression
 */
case class RightAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    p1.tryMatch(input, start, trace) match {
      case m@Matched(create1, rest1, tr1) => 
        p1.tryMatch(rest1, tr1.end, trace) match {
          case m2@Matched(create2, rest2, tr2) => 
            new Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end))
          case f => p2.tryMatch(rest1, tr1.end, trace)
        }
      case f => f
    }
  
  private def keepMatching(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    p1.tryMatch(input, start, trace) match {
      case Matched(create1, rest1, tr1) => 
        keepMatching(rest1, tr1.end, trace) match {
          case f: Failed => f
          case Matched(create2, rest2, tr2) =>
            Matched(() => ConsNode(create1(), create2()), rest2, tr1.start to tr2.end)
        }
      case f => p2.tryMatch(input, start, trace)
    }
  }
}

// case class BinaryOpPattern(p1: Pattern, op: TokenTypePattern, p2: Pattern)(leftAssoc: Boolean) 
//   extends Pattern {
//   private val fn = if (leftAssoc) leftAssocMatch else rightAssocMatch
//   override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
//     fn(input, start, trace)
//   def leftAssocMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
//     null
//   }
//   def rightAssocMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
//     null
//   }
// }

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
class PatternClass(override val name: String, private val patterns: Pattern*) 
    extends MultiPattern with INamedPattern {

//  override lazy val isFixed: Boolean = patterns.forall(_.isFixed)
//  override lazy val isEager: Boolean = patterns.exists(_.isEager)
  
  for (pattern <- patterns) pattern Extends this
  
  override def tryMatch(input: List[Tok], pos: Position, trace: Trace): ParseResult = {
    val tabbing = List.fill(trace.size)("  ").mkString //String.repeat(" ", trace.size)
    var skipped: Pattern|Null = null
    //println(s"\n$tabbing PatternClass, I'm $name, Trace = $trace")
    //println(s"$tabbing Input = ${input.map(_.text)}")
    val filtered = patterns
    //  (if (trace.nonEmpty) 
    //   patterns.filter(pattern =>
    //     if (pattern == (trace(trace.size - 1))) {
    //       println(s"$tabbing skipping $pattern");skipped=pattern;false} else true) 
    // else patterns)
    //println(s"$tabbing Filtered = $filtered")
    filtered.view.map(p => p.tryMatch(input, pos, {/*println(s"asdfasd$trace");*/trace}) match {
          case m: Matched[?, ?] => /*println(s"Matched $p!!");*/m
          case f => f
        })
        .find(_.isInstanceOf[Matched[?, ?]])
        .getOrElse(
          if (skipped != null) (skipped.asInstanceOf[Pattern]).tryMatch(input, pos, trace)
          else Failed(headOrEmpty(input), List.empty, pos))
  }
}

object PatternClass {
  def make(name: String, patterns: Pattern*): Unit = {
    Pattern.allPatterns.update(name, new PatternClass(name, patterns: _*))
  }
}


case class FunctionPattern(matchFun: (List[Tok], Position) => ParseResult
                              //    override val isFixed: Boolean = false,
                              //    override val isEager: Boolean = true
                          ) extends Pattern {
  val id = new scala.util.Random().nextInt().toHexString               
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    matchFun(input, start)
  override def toString: String = s"FunctionPattern$id"
}