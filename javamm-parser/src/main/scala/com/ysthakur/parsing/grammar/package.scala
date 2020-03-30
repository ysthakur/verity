package com.ysthakur.parsing

import com.ysthakur.parsing.lexer.RegexPattern

package object grammar {

  implicit class State(name: String) {
    def :=[Input, Helper](
        patternCases: PatternCase[Input, Helper]*
    )(implicit tokenizer: LexerOrParser[_, _, _]): Unit =
      tokenizer.addStateCase(
          StateCase(
              name,
              patternCases.asInstanceOf[Iterable[
                  PatternCase[tokenizer.I, tokenizer.Helper]
              ]]
          )
      )
  }

  implicit def toRegex(regex: String): RegexPattern = RegexPattern(regex)

  /*/**
 * Matches empty input
 * @param empty
 * @tparam E
 */
    implicit class EmptyPattern[E](empty: Unit) extends Pattern[E] {
        override val isFixed: Boolean = true
        override def tryMatch[T <: Iterable[E]](input: T): MatchResult = PartialMatch(EmptyMatch)
    }*/
}
