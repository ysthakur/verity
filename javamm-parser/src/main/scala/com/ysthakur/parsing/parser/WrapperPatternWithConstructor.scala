package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{Match, MatchResult, Pattern}
import com.ysthakur.parsing.parser.NodeCtor
import com.ysthakur.parsing.ast.Types._

abstract class PatternWithConstructor[C <: NodeCtor[_, _]](
  val ctor: C
) extends Pattern[Node]

case class WrapperPatternWithConstructor[C <: NodeCtor[_, _]](
  pattern: Pattern[Node],
  override val ctor: C
) extends PatternWithConstructor[C](ctor) {
  override val isFixed: Boolean = pattern.isFixed
  override def tryMatch(input: Iterable[Node]): MatchResult =
    pattern.tryMatch(input)
}