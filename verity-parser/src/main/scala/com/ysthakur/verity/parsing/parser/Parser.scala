package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.CompilationError
import com.ysthakur.verity.parsing.Position
import com.ysthakur.verity.parsing.ast.Node
import com.ysthakur.verity.parsing.lexer._

import scala.collection.mutable.ListBuffer

object Parser {
  @throws[CompilationError]
  def parse(tokens: List[Tok]): Node = {
    val blah = ParserPatterns.expr
    println("blahsdflkasdf")
    blah.tryMatch(tokens, Position(0, 0, 0), ListBuffer()) match {
      case Failed(got, exp, pos) => throw ParseError(exp, got, pos)
      case Matched(create, rest, _) =>
        if (rest.isEmpty) create() 
        else throw CompilationError(s"Matched ${create().text} \nbut could not match $rest")
    }
  }
}