package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.lexer.{TokenType, Token}

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
    override type AsNode = Token
    override type Input = Token

    override val isEager: Boolean = false
    override val isFixed: Boolean = true
    override def tryMatch(input: Iterable[Token], offset: Int): MatchResult = {
        if (input.size < 1)
            NeedsMore
        else if (input.head.tokenType == tokenType) {
            val matched = SingleMatch[Token](input.head, offset)
            if (input.size > 1) PartialMatch[Token](matched)
            else FullMatch[Token](matched, false)
        } else NoMatch
    }
    override def create(matched: MatchIn): this.AsNode = {
        ???
    }
    override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
    TokenTypePattern(tokenType)