package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.parsing.{Token, TokenType, Tok, Position, EmptyToken}

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
  
//  override val isEager: Boolean = false
//  override val isFixed: Boolean = true
  
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    if (input.size >= 1) input.head match {
      case token: Token[?] => if (token.tokenType == tokenType) 
          return Matched(() => token, input.tail, token.range)
    }
    val head = headOrEmpty(input)
    Failed(head, List.empty, if (head != EmptyToken) head.range.start else Position(0, -1, -1))
  }
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern = 
  TokenTypePattern(tokenType)