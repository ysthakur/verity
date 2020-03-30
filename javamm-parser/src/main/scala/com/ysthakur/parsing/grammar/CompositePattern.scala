package com.ysthakur.parsing.grammar

import com.ysthakur.util._

import scala.collection.mutable.ListBuffer

/**
  * Used to match first `pattern1`, then `pattern2`
  *
  * @tparam Input
  */
class CompositePattern[Input](
    patterns_ : Iterable[Pattern[Input]] = Iterable.empty
) extends Pattern[Input] {

  val patterns: ListBuffer[Pattern[Input]] =
    if (patterns_.isInstanceOf[ListBuffer[Pattern[Input]]])
      patterns_.as
    else new ListBuffer().addAll(patterns_.as)

  override val isFixed: Boolean = patterns.forall(_.isFixed)

  override def -[T <: Input](other: Pattern[T]): Pattern[Input] = {
    new CompositePattern(other match {
      case x: CompositePattern[Input] =>
        (patterns.concat[Pattern[Input]](x.patterns))
      case _ => patterns.appended[Pattern[Input]](other.as)
    })
  }

  override def tryMatch(input: Iterable[Input]): MatchResult = {
    val matches            = ListBuffer[PatternMatch[Input]]()
    var currentOffset      = 0
    var lastCouldMatchMore = false
    for (pattern <- patterns) {
      pattern.tryMatch(input.as) match {
        case full: FullMatch[Input] => return full
        case PartialMatch(matched) =>
          matches.addOne(
              PatternMatch(pattern, matched.matched, matched.start, matched.end)
          )
        case x => return x
      }
    }
    val match_ : Match[Input] = CompositeMatch(matches.toList)
    if (currentOffset < input.size) PartialMatch(match_)
    else FullMatch(match_, lastCouldMatchMore)
  }
}
