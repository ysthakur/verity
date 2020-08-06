package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.parsing.lexer.{TokenType, Tok, EmptyToken, Token}
import com.ysthakur.verity.parsing.Position

case class TokenTypePattern(val tokenType: com.ysthakur.verity.parsing.lexer.TokenType) extends Pattern {
  
//  override val isEager: Boolean = false
//  override val isFixed: Boolean = true
  
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult = {
    println(s"\nTrying to match token type $tokenType")
    if (input.size >= 1) input.head match {
      case token: Token[?] => if (token.tokenType == tokenType) 
          println(s"Matched $token")
          return Matched(() => token, input.tail, token.range)
    }
    println("didnt match :(\n")
    val head = headOrEmpty(input)
    Failed(head, List.empty, if (head != EmptyToken) head.range.start else Position(0, -1, -1))
  }
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern = 
  TokenTypePattern(tokenType)