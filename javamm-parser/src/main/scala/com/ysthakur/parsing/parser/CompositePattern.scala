package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.util._
import com.ysthakur.util.as
import com.ysthakur.util.utils

import scala.collection.mutable.ListBuffer
/*

class CompositePattern[I <: Node](
    patterns_ : Iterable[Pattern] = Iterable.empty,
    override val isEager: Boolean = true
) extends Pattern {
  override type Input = I

  val patterns: ListBuffer[Pattern] =
    patterns_ match {
      case value: ListBuffer[Pattern] => value
      case _ => new ListBuffer().addAll(patterns_)
    }

  override val isFixed: Boolean = patterns.forall(_.isFixed)

  // override def tryCreate(input: Iterable[Node], offset: Int): (ParseResult, scala.Option[this.AsNode]) = ???
  // override def -[T <: Input](other: Pattern): CompositePattern[Node] = {
  //   new CompositePattern(other match {
  //     case x: CompositePattern[Node] =>
  //       patterns.concat[Pattern](x.patterns)
  //     case _ => patterns.appended[Pattern](other.asInstanceOf)
  //   })
  // }

  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    val matches = ListBuffer[PatternMatch[Input]]()
    var currentOffset = 0
    var lastCouldMatchMore = false
    for (pattern <- patterns) {
      pattern.tryMatch(input.asInstanceOf, offset, trace) match {
        case FullMatch(matched, couldMatchMore) => {
          matches.addOne(PatternMatch(pattern, matched.matched.asInstanceOf[Iterable[Input]], matched.start, matched.end))
          lastCouldMatchMore = couldMatchMore
        }
        case PartialMatch(matched) => {
          matches.addOne(
            PatternMatch(pattern, matched.matched.asInstanceOf[Iterable[Input]], matched.start, matched.end)
          )
          currentOffset = matched.end
        }
        case x => return x
      }
    }
    //TODO FIX THIS!!!!!!!
    val match_ : Match[Node] = null.asInstanceOf//CompositeMatch(matches.toList)
    if (currentOffset < input.size) PartialMatch(match_)
    else FullMatch(match_, lastCouldMatchMore)
  }
  
  // override def ==(other: Pattern): Boolean = ???
  // override def copy: this.type = ???
  override def create(matched: MatchIn): AsNode = ???
}
*/
