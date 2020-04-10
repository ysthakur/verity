package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.util._
import com.ysthakur.util.as
import com.ysthakur.util.utils

import scala.collection.mutable.ListBuffer

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

  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, scala.Option[this.AsNode]) = ???
  // override def -[T <: Input](other: Pattern): CompositePattern[Input] = {
  //   new CompositePattern(other match {
  //     case x: CompositePattern[Input] =>
  //       patterns.concat[Pattern](x.patterns)
  //     case _ => patterns.appended[Pattern](other.asInstanceOf)
  //   })
  // }

  override def tryMatch(input: Iterable[this.Input], offset: Int, trace: Trace): MatchResult = {
    val matches = ListBuffer[PatternMatch[Input]]()
    var currentOffset = 0
    var lastCouldMatchMore = false
    for (pattern <- patterns) {
      pattern.tryMatch(input.asInstanceOf, offset, trace :+ this) match {
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
    val match_ : Match[Input] = CompositeMatch(matches.toList)
    if (currentOffset < input.size) PartialMatch(match_)
    else FullMatch(match_, lastCouldMatchMore)
  }
  
  // override def ==(other: Pattern): Boolean = ???
  // override def copy: this.type = ???
  override def create(matched: MatchIn): AsNode = ???
}
