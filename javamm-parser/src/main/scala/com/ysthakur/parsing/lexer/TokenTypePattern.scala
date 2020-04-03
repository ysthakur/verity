package com.ysthakur.parsing.lexer

import java.util.regex.Pattern

import com.ysthakur.parsing.grammar._

import scala.collection.immutable.WrappedString

object TokenTypePattern {

  type Str = CharSequence & Iterable[Char]

  def tryMatch(tokenType: TokenType, input: String, offset: Int): MatchResult = {
    (tokenType match {
      case ftt: FixedTextTokenType => tryMatchText(ftt.text)
      case rtt: RegexTokenType => tryMatchRegex(rtt.regex)
    })(input, offset)
  }

  def tryMatchText(text: String)(input: String, offset: Int): MatchResult = {
    val inLen = input.length
    val size = input.size
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

  def tryMatchRegex(regex: String)(input: String, start: Int): MatchResult = {
    val pattern = Pattern.compile(regex)
    val matcher = pattern.matcher(input)
    try {
      if (matcher.matches())
        FullMatch(
          ExactMatch(matcher.group(), TextRange(0, matcher.end)),
          matcher.requireEnd()
        )
      else if (matcher.find(0))
        PartialMatch(ExactMatch(matcher.group(), 0, matcher.end))
      else if (matcher.hitEnd()) NeedsMore()
      else NoMatch()
    } catch {
      case e: StackOverflowError => throw e
    }
  }
}
