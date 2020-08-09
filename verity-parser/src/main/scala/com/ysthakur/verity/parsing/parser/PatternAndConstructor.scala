package com.ysthakur.verity.parsing.parser

import com.ysthakur.verity.parsing.ast.infile.Node
import com.ysthakur.verity.parsing.lexer.Tok
import com.ysthakur.verity.parsing._

case class PatternAndConstructor[N <: Node](
    pattern: Pattern, ctor: Node => N
  ) extends Pattern {

//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def apply(input: List[Tok], start: Position, trace: Trace): ParseResult =
    pattern.tryMatch(input, start, trace) match {
      case Matched(create, rest, range) => Matched(() => ctor(create()), rest, range)
      case failed => failed
    }
}