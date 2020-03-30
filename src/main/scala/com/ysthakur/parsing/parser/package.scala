package com.ysthakur.parsing

import com.ysthakur.parsing.grammar.{Match, Pattern}
import com.ysthakur.parsing.lexer.TokenType

import scala.reflect.ClassTag

package object parser {
  type Node = com.ysthakur.parsing.ast.Node

  def p[N <: Node](implicit tag: ClassTag[N]): SingleNodePattern[N] = {
    //implicit val tag_ : ClassTag[N] = tag
    new SingleNodePattern[N]
  }

  implicit def toTokenTypePattern(tokenType: TokenType): TokenTypePattern =
    TokenTypePattern(tokenType)

  implicit class PatternWrapper(val pattern: Pattern[Node]) {
    def >>[N: ClassTag](
        implicit constructor: (_ <: Match[Node]) => Node
    ): PatternWithConstructor[Node, _] =
      PatternWithConstructor(pattern, constructor)
  }
}
