package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.grammar._
import com.ysthakur.parsing.lexer.{TokenType}
import com.ysthakur.parsing.parser._

case class TokenTypePattern(val tokenType: TokenType) extends Pattern[TokenType] {
    override val isEager: Boolean = false
    override val isFixed: Boolean = true
    override def tryMatch(input: Iterable[TokenType], offset: Int): MatchResult = {
        if (input.size < 1)
            NeedsMore()
        else if (input.head == tokenType) {
            val matched = SingleMatch[TokenType](tokenType, offset)
            if (input.size > 1) PartialMatch[TokenType](matched, offset, offset + 1)
            else FullMatch[TokenType](matched, false)
        } else NoMatch()
    }
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
    TokenTypePattern(tokenType)
