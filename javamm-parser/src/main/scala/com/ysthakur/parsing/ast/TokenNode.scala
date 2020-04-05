package com.ysthakur.parsing.ast

import com.ysthakur.parsing.lexer.{Token, TokenType}

trait TokenNode(override val tokenType: TokenType) extends Node {
  
}