package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{MatchResult, Pattern}
import com.ysthakur.parsing.lexer.TokenType

case class TokenTypePattern(tokenType: TokenType) extends Pattern[Node] {

  /**
    * Whether or not it always matches the same input.
    * If false, it might be a valid identifier or something
    * that takes a variable length input or something like that
    */
  override val isFixed: Boolean = false

  override def tryMatch(input: Iterable[Node]): MatchResult = ???
}
