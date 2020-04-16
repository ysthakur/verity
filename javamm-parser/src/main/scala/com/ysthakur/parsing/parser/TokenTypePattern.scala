package com.ysthakur.parsing.parser

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer._

case class TokenTypePattern(val tokenType: TokenType) extends Pattern {
  override type AsNode = Token[TokenType]
  type Tok = Token[tokenType.type]

  override val isEager: Boolean = false
  override val isFixed: Boolean = true
  override def tryMatch(input: List[Node], offset: Int, trace: Trace): ParseResult = {
    if (input.size >= 1) input.head match {
      case token: Token[?] => if (token.tokenType == tokenType) 
          return Matched(token, input.tail, offset + token.text.size)
    }
    return Failed( headOrEmpty(input), List.empty)
  }

  override def expected(prevRes: ParseResult): List[String] = List(tokenType.toString)
}

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
TokenTypePattern(tokenType)