package com.ysthakur.parsing.grammar

import com.ysthakur.parsing._
import com.ysthakur.util._
import com.ysthakur.util.as
import com.ysthakur.util.utils

import scala.collection.mutable.ListBuffer


class CompositePattern[Input](
    patterns_ : Iterable[Pattern[Input]] = Iterable.empty
) extends Pattern[Input] {

  val patterns: ListBuffer[Pattern[Input]] =
    patterns_ match {
      case value: ListBuffer[Pattern[Input]] => value
      case _ => new ListBuffer().addAll(patterns_)
    }

  override val isFixed: Boolean = patterns.forall(_.isFixed)

  override def -[T <: Input](other: Pattern[T]): CompositePattern[Input] = {
    new CompositePattern(other match {
      case x: CompositePattern[Input] =>
        patterns.concat[Pattern[Input]](x.patterns)
      case _ => patterns.appended[Pattern[Input]](other.asInstanceOf)
    })
  }

  override def tryMatch(input: Iterable[Input], offset: Int): MatchResult = {
    val matches            = ListBuffer[PatternMatch[Input]]()
    var currentOffset      = 0
    var lastCouldMatchMore = false
    for (pattern <- patterns) {
      pattern.tryMatch(input.asInstanceOf, offset) match {
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
}
