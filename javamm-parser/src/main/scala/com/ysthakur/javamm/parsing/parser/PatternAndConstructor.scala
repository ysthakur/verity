package com.ysthakur.javamm.parsing.parser

import com.ysthakur.javamm.parsing.ast._
import com.ysthakur.javamm.parsing._
import com.ysthakur.javamm.parsing.ast.infile.Node
import com.ysthakur.javamm.parsing.lexer.{Match, Tok}

case class PatternAndConstructor[N <: Node](
    pattern: Pattern, ctor: Node => N
  ) extends Pattern {

//  override def isFixed: Boolean = pattern.isFixed
//  override def isEager: Boolean = pattern.isEager
  override def tryMatch(input: List[Tok], start: Position, trace: Trace): ParseResult =
    pattern.tryMatch(input, start, trace) match {
      case Matched(create, rest, range) => Matched(() => ctor(create()), rest, range)
      case failed => failed
    }
}