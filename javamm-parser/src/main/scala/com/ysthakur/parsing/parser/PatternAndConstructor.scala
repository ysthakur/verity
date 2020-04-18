package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast._
import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.infile.TextNode
import com.ysthakur.parsing.lexer.{Match, Tok}

case class PatternAndConstructor[N <: TextNode](
    pattern: Pattern, ctor: TextNode => N
  ) extends Pattern {

  override def isFixed: Boolean = pattern.isFixed
  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Tok], offset: Int, trace: Trace): ParseResult =
    pattern.tryMatch(input, offset, trace) match {
      case Matched(create, rest, offset) => Matched(() => ctor(create()), rest, offset)
      case failed => failed
    }
}