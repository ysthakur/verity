package com.ysthakur.parsing.grammar

case class StateCase[Input, Helper <: LexerOrParserHelper[_, _, _, _]](
    state: String,
    patternCases: Iterable[PatternCase[Input, Helper]],
    onError: (Any, Helper#A) => Any =
      (pos, acc) => s"Bad character $acc found at $pos"
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
