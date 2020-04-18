package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer._

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
  
  override val isEager: Boolean = false
  override val isFixed: Boolean = true
  
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult = {
    if (input.size >= 1) input.head match {
      case token: Token[?] => if (token.tokenType == tokenType) 
          return Matched(() => token, input.tail, offset + token.text.size)
    }
    val head = headOrEmpty(input)
    Failed(head, List.empty, if (head != EmptyToken) head.pos else Position(0, -1, -1))
  }
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern = 
  TokenTypePattern(tokenType)