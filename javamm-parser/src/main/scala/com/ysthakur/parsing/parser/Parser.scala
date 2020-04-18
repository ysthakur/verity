package com.ysthakur.parsing.parser

import com.ysthakur.CompilationError
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.ast.infile.TextNode
import com.ysthakur.parsing.lexer.{Tok, Token, TokenType}

import scala.collection.mutable.ListBuffer

object Parser {
  @throws[CompilationError]
  def parse(tokens: List[Tok]): TextNode = {
    ParserPatterns.root.tryMatch(tokens, 0, ListBuffer()) match {
      case Failed(got, exp, pos) => throw ParseError(exp, got, pos)
      case Matched(create, rest, _) => if (rest.isEmpty) create() else throw new Error(s"Could not match $rest")
    }
  }
}