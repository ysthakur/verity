package com.ysthakur.parsing.parser

import com.ysthakur.util._
import com.ysthakur.parsing._
import com.ysthakur.parsing.ast.Types._

case class FunctionPattern[I <: Node, M <: Match[?], N <: Node](
    matchFun: (_ <: Iterable[I], Int) => MatchResult,
    ctor: (M) => N,
    override val isFixed: Boolean = false,
    override val isEager: Boolean = true
) extends Pattern {
  override type Input = I
  override type AsNode = N
  override type MatchIn = M
  override def create(matched: MatchIn): this.AsNode = ctor(matched)
  override def tryMatch(input: Iterable[this.Input], offset: Int): MatchResult =
    matchFun(input.asInstanceOf, offset)
  override def tryCreate(input: Iterable[Input], offset: Int): Either[MatchResult, this.AsNode] = ???
}

// implicit def toFunctionPattern[Input]
//   (matchFun: (_ <: Iterable[Input], Int) => MatchResult): FunctionPattern[Input] =
//     FunctionPattern(matchFun)