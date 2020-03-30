package com.ysthakur.parsing

import com.ysthakur.parsing.grammar.{Match, Pattern}
import com.ysthakur.parsing.lexer.TokenType
import com.ysthakur.util._

import scala.reflect.ClassTag

package object parser {
  type Node = com.ysthakur.parsing.ast.Node

  def p[N <: Node](implicit tag: ClassTag[N]): SingleNodePattern[N] = {
    //implicit val tag_ : ClassTag[N] = tag
    new SingleNodePattern[N]
  }

  implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
    TokenTypePattern(tokenType)

  implicit class PatternWrapper[N <: Node](val pattern: Pattern[Node]) {
    def >>[C <: NodeCtor[_ <: Match[Node], _]](
        implicit tag: ClassTag[C#In],
        ctor: C
    ): WrapperPatternWithConstructor[C] =
      WrapperPatternWithConstructor(pattern.as/*[Pattern[C#Input]]*/, ctor)
  }
}
