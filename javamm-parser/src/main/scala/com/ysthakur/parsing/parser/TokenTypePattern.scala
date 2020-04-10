package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.lexer.{TokenType, Token}

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
    override type AsNode = Token[TokenType]
    override type Input = Token[TokenType]
    type Tok = Token[tokenType.type]

    override val isEager: Boolean = false
    override val isFixed: Boolean = true
    override def tryMatch(input: Iterable[Token[?]], offset: Int, trace: Trace): MatchResult = {
        if (input.size < 1)
            NeedsMore
        else if (input.head.tokenType == tokenType) {
            val matched = SingleMatch[Tok](input.head.asInstanceOf[Tok], offset)
            if (input.size > 1) PartialMatch[Tok](matched)
            else FullMatch[Tok](matched, false)
        } else NoMatch
    }
    override def create(matched: MatchIn): this.AsNode = {
        ???
    }
    // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, scala.Option[this.AsNode]) = ???
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
    TokenTypePattern(tokenType)