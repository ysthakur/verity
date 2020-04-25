package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.CompilationError
import com.ysthakur.javamm.parsing.Position
import com.ysthakur.javamm.parsing.ast._
import com.ysthakur.javamm.parsing.{Tok, TokenType}

import scala.collection.mutable.ListBuffer

object Parser {
  @throws[CompilationError]
  def parse(tokens: List[Tok]): Node = {
    ParserPatterns.root.tryMatch(tokens, Position(0, 0, 0), ListBuffer()) match {
      case Failed(got, exp, pos) => throw ParseError(exp, got, pos)
      case Matched(create, rest, _) => 
        if (rest.isEmpty) create() 
        else throw CompilationError(s"Matched ${create()} \nbut could not match $rest")
    }
  }
}