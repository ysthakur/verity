package verity.parsing.parser

import verity.{CompilationError, Lazy}
import verity.parsing.{Token, TokenType, TextRange}
import verity.parsing.ast._
import verity.parsing.ast.infile._
// import verity.parsing.lexer.{InvariantToken, Tok, Token}

import scala.collection.mutable.ListBuffer
import scala.Option
import scala.collection.mutable
import scala.util.control.Breaks._

// type Trace = ListBuffer[(TextRange|Null, INamedPattern)]

/**
  * A pattern, like regex, that matches reader
  *
  * @tparam reader The type of the reader (Iterable of Char or Token)
  */
trait Pattern {
  
  type Out

  /**
    * Whether or not it always matches the same reader.
    * If false, it might be a valid identifier or something
    * that takes a variable length reader or something like that
    */
//  def isFixed: Boolean
//  def isEager: Boolean
  var _superPattern: INamedPattern|Null = null
  def superPattern: INamedPattern|Null = _superPattern

  def apply(reader: Reader): ParseResult[this.Out]

  /*inline*/ def tryMatch(reader: Reader): ParseResult[this.Out] =
    try {
      Pattern.indent += 2
      val res = apply(reader)
      Pattern.indent -= 2
      res
    } catch {
      case e if !e.isInstanceOf[CompilationError] => 
        throw new CompilationError(s"Exception on reader ${headOrEmpty(reader)}", e)
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

  def -(s: String): Pattern.Aux[Out] =
    reader => tryMatch(reader) match {
      case m: Matched[?] => 
        reader.nextToken(s, TokenType.MISC, _.text == s) match {
          case None => Failed(Token.empty(m.range.start), List(s), m.range.start)
          case _ => m
        }
      case f => f
    }

  def |[T <: Pattern](other: T): Pattern.Aux[this.Out | other.Out] =
    OrPattern(this, other).asInstanceOf[Pattern.Aux[this.Out | other.Out]]

  def * : Pattern.Aux[List[this.Out]] = RepeatPattern[this.type](this/*, isEager = true*/)
  // def *\ : Pattern.Aux[this.type] = RepeatPatternRev(this)
  //def *? : Pattern = RepeatPattern(this/*, isEager = false*/)
  def ? = MaybePattern(this)

  def repeat(min: Int=0, max: Int=Int.MaxValue) = 
    RepeatPattern(this, min, max)

  def Extends(superPattern: INamedPattern): this.type = {
    this._superPattern = superPattern
    this
  }

  //def doesExtend(superPattern: PatternClass): Boolean = this.superPattern == superPattern
  def subOf(other: Pattern): Boolean = other == this.superPattern
  //def ==(other: Pattern): Boolean = ???
  def headOrEmpty(reader: Reader): Token = ???
    // reader.nextToken().getOrElse(Token(TextRange.empty(reader.offset), "", TokenType.MISC))

  def |>[N](ctor: Out => N): PatternAndConstructor[Out, N] = PatternAndConstructor(this, ctor)

  def println(s: Any): Unit = {} //System.out.println(""+Pattern.indent+"  ".repeat(Pattern.indent) + s)
}

def (patternName: String) := (pattern: => Pattern): Unit = {

Pattern.allPatterns.put(patternName, pattern)
}

object Pattern {
  //type -[A <: Pattern, B <: Pattern] = ConsPattern[A, B]
  //type ||[A <: Pattern, B <: Pattern] = OrPattern[A, B]
  //type *[T <: Pattern] = RepeatPattern
  //type *?[T <: Pattern] = RepeatPattern

  type Aux[N] = Pattern { type Out = N }

  val allPatterns: mutable.LinkedHashMap[String, Pattern] = mutable.LinkedHashMap()
  var indent: Int = 0

  def headOrEmpty(reader: Reader): Token = ???
    // reader.nextToken().getOrElse(Token(TextRange.empty(reader.offset), "", TokenType.MISC))

  def fromOption(optPattern: Reader => Option[Token], expected: List[String] = Nil): Pattern.Aux[Token] =
    reader => {
        val start = reader.offset
        optPattern(reader) match {
        case Some(token) => Matched(() => token, reader, token.textRange)
        case None => Failed(headOrEmpty(reader), expected, start)
      }
    }

}

// implicit def toNamedPattern(patternName: String): INamedPattern = PatternRef(patternName)

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
/* case class PatternRef(override val name: String) extends INamedPattern {
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
  override def apply(reader: Reader): ParseResult[Out] = {
    try {
      //println(s"I am $name, Trace = $trace, reader = ${headOrEmpty(reader)}")
      val x = 9;
      //TODO fix this with a variable or something
      if (trace.size > 100) throw new CompilationError(s"Trace is too big (${trace.mkString(",")}")
      if (trace.nonEmpty && isLeftRecursive(trace, textRangeToEnd(start, reader))) {
        //println("\tAnd I have failed")
        return Failed(headOrEmpty(reader), List(), start)
      }
      val p = pattern
      val newTrace = if (trace.nonEmpty && trace.last._2.subOf(this)) trace else trace :+ (textRangeToEnd(reader), this)
      p.tryMatch(reader, newTrace) match {
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
} */

def textRangeToEnd(reader: Reader): TextRange = TextRange(reader.offset, reader.length)

object - {
  def unapply[A, B](arg: ConsNode[A, B]): Option[(A, B)] = {
    Some(arg.n1, arg.n2)
  }
}

// object || {
//   def unapply[A <: Node, B <: Node](arg: OrNode[A, B]): Option[(Node, Node)] = {
//     arg match {
//       case LeftNode(left) => Some((left, EmptyNode))
//       case RightNode(right) => Some((EmptyNode, right))
//     }
//   }
// }

/**
 * Like RepeatPattern, but **immediately** evaluates and tries to fold left.
 */
class FoldLeft[A, B >: A](p1: Pattern.Aux[A], p2: Pattern, min: Int = 0, max: Int = Int.MaxValue)(combine: (B, p2.Out) => B) extends Pattern {
  type Out = B
  override def apply(reader: Reader): ParseResult[Out] = {
    val start = reader.offset
    p1.tryMatch(reader) match {
      case Matched(create, _, range1) =>
        var acc: B = create()
        var numMatches = -1
        var end = range1.end
        while (numMatches < max) {
          p2.tryMatch(reader) match {
            case Matched(create2, _, range2) =>
              acc = combine(acc, create2())
              end = range2.end
            case f: Failed =>
              if (numMatches < min) return f
              numMatches = max
          }
          numMatches += 1
        }
        if (numMatches == -1) Failed(Token.empty(range1.start), List(), range1.start)
        else {
          val finalAcc = acc
          Matched(() => finalAcc, reader, TextRange(start, end))
        }
      case f => f
    }
  }
}

case class ConsPattern[+T1 <: Pattern, +T2 <: Pattern](p1: T1, p2: T2, val name: String = "") extends Pattern {
  type Out = ConsNode[p1.Out, p2.Out]
//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager && p2.isEager
  
  override def apply(reader: Reader): ParseResult[Out] = {
    // println("--------------------")
    // println(s"incons name=$name, reader=${reader.map(_.text)}")
    if (!name.isEmpty) System.out.println("----------------------------\n")

    p1.tryMatch(reader) match {
      case Matched(create, rest, range) =>
        println("~~~~~~~~~~~~~~~")
        //println(s"Matched pattern 1, now matching $rest")
        p2.tryMatch(rest) match {
        case Matched(create2, rest2, range2) =>
          //println(s"\nMatched conspattern!!!, \n\t reader=$reader \n rest2=$rest")
          // println(s"Matched, name=$name, rest2=${rest2.map(_.text)}")
          // if (!name.isEmpty) System.out.println(s"$name matched! reader=$reader")
          Matched(() => ConsNode[p1.Out, p2.Out](create().asInstanceOf[p1.Out], create2().asInstanceOf[p2.Out]), rest2, TextRange(range.start, range2.end))
        case failed: Failed =>
          println(s"Didn't match conspattern, name=$name failed=$failed")
          // if (!name.isEmpty) System.out.println(s"$name failed1=$failed, rest=$reader")
          failed
      }
      case failed: Failed =>
        println(s"Didn't match cons 2, name=$name failed=$failed")
        // if (!name.isEmpty) System.out.println(s"$name failed2=$failed, reader=$reader")
        failed
    }
  }
}

case class OrPattern(p1: Pattern, p2: Pattern, shouldFlatten: Boolean = true) extends Pattern {
  type Out = p1.Out | p2.Out

//  override def isFixed: Boolean = p1.isFixed && p2.isFixed
//  override def isEager: Boolean = p1.isEager || p2.isEager
  // lazy val p2 = _p2

  override def apply(reader: Reader): ParseResult[Out] =
    p1.tryMatch(reader).or(p2.tryMatch(reader))
}

case class MaybePattern(pattern: Pattern) extends Pattern {
  type Out = pattern.Out | Null
  override def apply(reader: Reader): ParseResult[Out] =
    val start = reader.offset
    pattern
      .tryMatch(reader)
      .or(Matched(() => null, reader, TextRange.empty(start)))
}

/**
 * @param p1 The first part (an expression)
 * @param p2 The second part (some binary operator and another expression)
 */
/* case class LeftAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def apply(reader: Reader): ParseResult[Out] =
    p1.tryMatch(reader) match {
      case m@Matched(create1, rest1, tr1) => 
        // println(s"Matched first, create1=${create1()}")
        p2.tryMatch(rest1) match {
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

  def keepMatching(lastRes: Matched[_, _]): ParseResult[Out] = {
    val Matched(create1, rest1, tr1) = lastRes
    p2.tryMatch(rest1) match {
      case m2@Matched(create2, rest2, tr2) => 
        // println(s"Matched m2, create2=${create2()}")
        keepMatching(Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end)))
      case f => 
        // println(s"Failed, lastRes=${lastRes.create()}")
        lastRes
    }
  }
} */

/**
 * @param p1 The first part, e.g., an expression and some binary operator
 * @param p2 The second part, e.g., expression
 */
/* case class RightAssocPattern(p1: Pattern, p2: Pattern) extends Pattern {
  override def apply(reader: Reader): ParseResult[Out] =
    p1.tryMatch(reader) match {
      case m@Matched(create1, rest1, tr1) => 
        p1.tryMatch(rest1) match {
          case m2@Matched(create2, rest2, tr2) => 
            new Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end))
          case f => p2.tryMatch(rest1)
        }
      case f => f
    }
  
  // private def keepMatching(reader: Reader): ParseResult[Out] = {
  //   p1.tryMatch(reader) match {
  //     case Matched(create1, rest1, tr1) => 
  //       keepMatching(rest1, tr1.end) match {
  //         case f: Failed => f
  //         case Matched(create2, rest2, tr2) =>
  //           Matched(() => ConsNode(create1(), create2()), rest2, TextRange(tr1.start, tr2.end))
  //       }
  //     case f => p2.tryMatch(reader)
  //   }
  // }
} */

/**
  *
  */
case class RepeatPattern[P <: Pattern](
    pattern: P,
    min: Int = 0,
    max: Int = Int.MaxValue,
    name: String = ""
//    override val isEager: Boolean
) extends Pattern {

  type Out = List[pattern.Out]

  private def makeNodeList(results: List[() => pattern.Out], start: Int, end: Int, rest: Reader): Matched[Out] = {
    println(s"name=$name, results are ${results.map(_())}")
    val rev = results.reverse
    Matched(() => rev.map(_()), rest, TextRange(start, end))
  }

  override final def apply(reader: Reader) = {
    // println("--------------------")
    // println(s"inrepeat name=$name, reader=${reader.map{_.text}}")
    if (reader.isEmpty) println("\n\n\nreader is empty!!!!!!!!")

    @annotation.tailrec
    def recMatch(reader: Reader, start: Int, end: Int, count: Int, prev: List[() => pattern.Out]): ParseResult[Out] = {
      // println(s"reader=${reader.map(_.text)}")
      if (reader.isEmpty) return makeNodeList(prev, start, end, reader)
      pattern.tryMatch(reader) match {
        case m@Matched(create, rest, range) =>
          // println(s"\nprev=$prev\n")
          if (count < max) recMatch(rest, start, range.end, count + 1, create :: prev)
          else makeNodeList(create :: prev, start, range.end, rest)
        case f@Failed(got, expected, pos) =>
          if (count < min) f
          else makeNodeList(prev, start, end, reader)
      }
    }

    recMatch(reader, reader.offset, reader.offset, 0, List.empty)
  }
}

/**
 * A pattern that matches any one of a group of patterns
 */
/* class PatternClass(override val name: String, private val patterns: Pattern*) 
    extends INamedPattern {

//  override lazy val isFixed: Boolean = patterns.forall(_.isFixed)
//  override lazy val isEager: Boolean = patterns.exists(_.isEager)
  
  for (pattern <- patterns) pattern Extends this
  
  override def apply(reader: Reader, pos: Int): ParseResult[Out] = {
    val tabbing = List.fill(trace.size)("  ").mkString //String.repeat(" ", size)
    var skipped: Pattern|Null = null
    //println(s"\n$tabbing PatternClass, I'm $name, Trace = $trace")
    //println(s"$tabbing reader = ${reader.map(_.text)}")
    val filtered = patterns
    //  (if (trace.nonEmpty) 
    //   patterns.filter(pattern =>
    //     if (pattern == (trace(trace.size - 1))) {
    //       println(s"$tabbing skipping $pattern");skipped=pattern;false} else true) 
    // else patterns)
    //println(s"$tabbing Filtered = $filtered")
    filtered.view.map(p => p.tryMatch(reader, pos, {/*println(s"asdfasd$trace");*/trace}) match {
          case m: Matched[?, ?] => /*println(s"Matched $p!!");*/m
          case f => f
        })
        .find(_.isInstanceOf[Matched[?, ?]])
        .getOrElse(
          if (skipped != null) (skipped.asInstanceOf[Pattern]).tryMatch(reader, pos)
          else Failed(headOrEmpty(reader), List.empty, pos))
  }
}

object PatternClass {
  def make(name: String, patterns: Pattern*): Unit = {
    Pattern.allPatterns.update(name, new PatternClass(name, patterns: _*))
  }
} */


case class FunctionPattern[N](matchFun: Reader => ParseResult[N]) extends Pattern {
  type Out = N
  val id = new scala.util.Random().nextInt().toHexString               
  override def apply(reader: Reader) = matchFun(reader)
  override def toString: String = s"FunctionPattern$id"
}