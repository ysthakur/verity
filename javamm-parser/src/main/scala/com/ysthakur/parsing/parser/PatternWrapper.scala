package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{Match, Pattern}
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.{TokenType}

import scala.language.implicitConversions
import scala.reflect.ClassTag

//type Node = com.ysthakur.parsing.ast.Node

def p[N <: Node](implicit tag: ClassTag[N] ): SingleNodePattern[N] =
  new SingleNodePattern[N]

implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
TokenTypePattern (tokenType)

class PatternWrapper[N <: Node](val pattern: Pattern[N]) {
  // def >>[C <: NodeCtor[_ <: Match[Node], _]](
  //                             implicit tag: ClassTag[C#In],
  //                             ctor: C): WrapperPatternWithConstructor[N, C] =
  //   WrapperPatternWithConstructor[N, C](pattern, ctor)
}

implicit def toPatternWrapper[N <: Node](pattern: Pattern[N]): PatternWrapper[N] =
  PatternWrapper(pattern)
