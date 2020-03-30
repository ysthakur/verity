package com.ysthakur.parsing.grammar

import scala.language.existentials

case class StateCase[Input, Helper](
    state: String,
    patternCases: Iterable[PatternCase[Input, Helper]]
)

/**
  *
  * @param pattern
  * @param action
  */
case class PatternCase[Input, Helper](
    pattern: Pattern[Input],
    action: Helper => Unit
) {
  override def toString: String = s"PatternCase(pattern=$pattern,)"
}
