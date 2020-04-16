package com.ysthakur.parsing.parser

import com.ysthakur.CompilationError
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer.{Token, TokenType}

import scala.collection.mutable.ListBuffer

object Parser {
  
  @throws[CompilationError]
  def parse(tokens: List[Token[TokenType]]): Node = {
    ParserPatterns.root
        .tryMatch(tokens, 0, ListBuffer[NamedPattern[Node, Node]]()) match {
      case Failed(msg) => throw new CompilationError("Failed() for some reason")
      case Matched(create, _, _) => create()
    }
  }
  
}
