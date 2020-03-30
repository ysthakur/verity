package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.grammar._

case class FixedTextPattern(text: String) extends Pattern[Char] {
  private lazy val size = text.length

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = true

  override def tryMatch(inputChars: Iterable[Char]): MatchResult = {
    val input = toStr(inputChars)
    val inLen = input.length
    if (input == text)
      return FullMatch(
          ExactMatch(input, TextRange(0, size)),
          couldMatchMore = false
      )
    else if (inLen < size) {
      if (text.startsWith(input)) return NeedsMore()
    } else if (inLen > size) {
      if (input.indexOf(text) == 0)
        return PartialMatch(ExactMatch(input, 0, size))
    }
    NoMatch()
  }
}
