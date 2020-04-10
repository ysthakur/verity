package com.ysthakur.parsing.parser

import com.ysthakur.util._
import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._

import scala.Option
import scala.collection.mutable.ListBuffer

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
  override def tryMatch(input: Iterable[this.Input], offset: Int, trace: Trace): MatchResult =
    matchFun(input.asInstanceOf, offset)
  override def ==(other: Pattern): Boolean = this.equals(other)
  //override def copy: Pattern = FunctionPattern(matchFun, ctor, )
  // override def tryCreate(input: Iterable[Input], offset: Int): (MatchResult, Option[this.AsNode]) = ???
}

// implicit def toFunctionPattern[Input]
//   (matchFun: (_ <: Iterable[Input], Int) => MatchResult): FunctionPattern[Input] =
//     FunctionPattern(matchFun)