package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.parsing

import parsing.lexer.{TokenType, Tok, Token, SymbolTokenType}
import parsing.Position

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
  
//  override val isEager: Boolean = false
//  override val isFixed: Boolean = true
  
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    //if (tokenType == SymbolTokenType.LSQUARE || tokenType == SymbolTokenType.RSQUARE) System.out.println(s"\nReached $tokenType input=$input")
    if (input.size >= 1) input.head match {
      case token: Token[?] => if (token.tokenType == tokenType) 
          // println(s"Matched $token")
          return Matched(() => token, input.tail, token.textRange)
    }
    if (tokenType == SymbolTokenType.LSQUARE || tokenType == SymbolTokenType.RSQUARE) System.out.println("didnt match :(\n")
    val head = headOrEmpty(input)
    Failed(head, List.empty, head.textRange.start)
  }
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern = 
  TokenTypePattern(tokenType)