package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.{CompilationError, Lazy}
import com.ysthakur.verity.parsing.ast.infile._
import com.ysthakur.verity.parsing.ast._
import com.ysthakur.verity.parsing.{Position, TextRange}
import com.ysthakur.verity.parsing.lexer.{EmptyToken, Tok, Token}

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

  def apply(input: List[Tok], start: Position, trace: Trace): ParseResult

  inline def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    try {
      Pattern.indent += 2
      val res = apply(input, start, trace)
      Pattern.indent -= 2
      res
    } catch {
      case e if !e.isInstanceOf[CompilationError] => 
        throw new CompilationError(s"Exception on input ${headOrEmpty(input)}", e)
    }

  /**
    * Just to compose multiple patterns. Match this pattern first, then
    * pattern `other`. It's left associative
    *
    * @param other
    * @return
    */
  def -[T <: Pattern](other: => T): ConsPattern[this.type, T] =
    new ConsPattern[this.type, T](this, other)

  def |[T <: Pattern](other: => T): OrPattern[this.type, T] =
    OrPattern(this, other)

  // def ||[T <: Pattern](other: T): OrPattern[this.type, T] =
  //   OrPattern(this, other, shouldFlatten=false)

  def * : Pattern  = RepeatPattern(this/*, isEager = true*/)
  //def *? : Pattern = RepeatPattern(this/*, isEager = false*/)
  def ? : Pattern = RepeatPattern(this, max=1)

  def repeat(min: Int=0, max: Int=Int.MaxValue) = 
    RepeatPattern(this, min, max)

  def Extends(superPattern: INamedPattern): this.type = {
    this._superPattern = superPattern
    this
  }

  //def doesExtend(superPattern: PatternClass): Boolean = this.superPattern == superPattern
  def subOf(other: Pattern): Boolean = other == this.superPattern
  //def ==(other: Pattern): Boolean = ???
  def headOrEmpty(it: Iterable[Tok]): Tok = if (it.isEmpty) EmptyToken else it.head

  def println(s: Any) = System.out.println(""+Pattern.indent+"  ".repeat(Pattern.indent) + s)
}

def (patternName: String) := (pattern: => Pattern): Unit = {

Pattern.allPatterns.put(patternName, pattern)
}

object Pattern {
  //type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  //type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  //type *[T <: Pattern] = RepeatPattern
  //type *?[T <: Pattern] = RepeatPattern

  val allPatterns: mutable.LinkedHashMap[String, Pattern] = mutable.LinkedHashMap()
  var indent: Int = 0
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
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult = {
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

class ConsPattern[T1 <: Pattern, T2 <: Pattern](p1: T1, _p2: => T2, val name: String = "") extends Pattern {
  
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager && p2.isEager

  lazy val p2 = _p2
  
  override inline def apply(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    /*p2 match {
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
      case _ =>*/
        println("--------------------")
        println(s"incons name=$name, input=${input.map(_.text)}")
        p1.tryMatch(input, start, trace) match {
          case Matched(create, rest, range) =>
            println("~~~~~~~~~~~~~~~")
            //println(s"Matched pattern 1, now matching $rest")
            p2.tryMatch(rest, range.end, ListBuffer()) match {
            case Matched(create2, rest2, range2) =>
              //println(s"\nMatched conspattern!!!, \n\t input=$input \n rest2=$rest")
              println(s"Matched, name=$name, rest2=${rest2.map(_.text)}")
              Matched(() => ConsNode(create(), create2()), rest2, TextRange(start, range2.end))
            case failed =>
              println(s"Didn't match conspattern, name=$name failed=$failed")
              failed
          }
          case failed =>
            println(s"Didn't match cons 2, name=$name failed=$failed")
            failed
        }
    //}
  }
}

class OrPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, _p2: => T2, shouldFlatten: Boolean = true) extends Pattern {
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager || p2.isEager
  lazy val p2 = _p2

  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
    p1.tryMatch(input, start, trace) match {
      case Failed(_) => p2.tryMatch(input, start, trace)
      case m => m
    }
}

case class MaybePattern(pattern: Pattern) extends Pattern {
//  override val isEager: Boolean = false
//  override val isFixed: Boolean = false
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
    // println(s"\nTrying to match $input")
    val x = pattern.tryMatch(input, start, trace)
    // println(s"matched $x")
    x.orElse(Matched(() => EmptyNode, input, TextRange.empty(start), true))
}

/**
 * @param p1 The first part (an expression)
 * @param p2 The second part (some binary operator and another expression)
 */
case class LeftAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
    p1.tryMatch(input, start, trace) match {
      case m@Matched(create1, rest1, tr1) => 
        // println(s"Matched first, create1=${create1()}")
        p2.tryMatch(rest1, tr1.end, trace) match {
          case m2@Matched(create2, rest2, tr2) => 
            // println(s"Iin tryMatch, create2=${create2()}")
            keepMatching(Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end)))
          case f => 
            // println("Failed on second")
            f
        }
      case f => 
        // println(s"Failed p1, boohoo")
        f
    }

  def keepMatching(lastRes: Matched[_, _]): ParseResult = {
    val Matched(create1, rest1, tr1) = lastRes
    p2.tryMatch(rest1, tr1.end, ListBuffer()) match {
      case m2@Matched(create2, rest2, tr2) => 
        // println(s"Matched m2, create2=${create2()}")
        keepMatching(Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end)))
      case f => 
        // println(s"Failed, lastRes=${lastRes.create()}")
        lastRes
    }
  }
}

/**
 * @param p1 The first part, e.g., an expression and some binary operator
 * @param p2 The second part, e.g., expression
 */
case class RightAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
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
//   override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
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
    min: Int = 0,
    max: Int = Int.MaxValue,
    name: String = ""
//    override val isEager: Boolean
) extends Pattern {
  private def makeNodeList(results: List[ParseResult], end: Position, rest: List[Tok]) = {
    println(s"name=$name, results are ${results.map{x=>x.asInstanceOf[Matched[?, ?]].create()}}")
    Matched(
      () => NodeList(results.map(r => r.asInstanceOf[Matched[?, ?]].create())),
      rest,
      TextRange(results match {
        case Matched(_, _, range) :: _ => range.start
        case _ => end
      }, end))
  }

  @annotation.tailrec
  private def recMatch(input: List[Tok], start: Position, trace: Trace, count: Int, prev: List[ParseResult]): ParseResult = {
    println(s"input=${input.map(_.text)}")
    if (input.isEmpty) return makeNodeList(prev.reverse, start, input)
    pattern.tryMatch(input, start, trace) match {
      case m@Matched(create, rest, range) =>
        // println(s"\nprev=$prev\n")
        if (count < max) recMatch(rest, range.end, trace, count + 1, m :: prev)
        else makeNodeList((m :: prev).reverse, m.range.end, rest)
      case f@Failed(got, expected, pos) =>
        if (count < min) f
        else makeNodeList(prev.reverse, start, input)
    }
  }

  override final def apply(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    println("--------------------")
    println(s"inrepeat name=$name, input=${input.map{_.text}}")
    if (input.isEmpty) println("\n\n\nINput is empty!!!!!!!!")
    recMatch(input, start, trace, 0, List.empty)
  }
}

/**
 * A pattern that matches any one of a group of patterns
 */
class PatternClass(override val name: String, private val patterns: Pattern*) 
    extends INamedPattern {

//  override lazy val isFixed: Boolean = patterns.forall(_.isFixed)
//  override lazy val isEager: Boolean = patterns.exists(_.isEager)
  
  for (pattern <- patterns) pattern Extends this
  
  override def apply(input: List[Tok], pos: Position, trace: Trace): ParseResult = {
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
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
    matchFun(input, start)
  override def toString: String = s"FunctionPattern$id"
}