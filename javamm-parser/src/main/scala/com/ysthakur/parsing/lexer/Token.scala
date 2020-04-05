package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.{ExactMatch, Match}

/**
  *
  * @param tokenType The type of this token
  * @param startOffset The index in the file where this token starts
  * @param endOffset The index in the file <em>before</em> which this token ends
  */
sealed abstract class Token(val tokenType: TokenType,
                     val startOffset: Int,
                     val endOffset: Int) extends Node with HasText

object Token {
}

/**
  * A token whose text is always the same
  *
  * @param tokenType
  */
case class InvariantToken(
    override val tokenType: FixedTextTokenType,
    override val startOffset: Int = -1,
    override val endOffset: Int = -1
) extends Token(tokenType, startOffset, endOffset) {
  override val text: String = tokenType.text
}

/**
  * A token whose text may be different, like a string literal
  *
  * @param tokenType
  */
case class VariantToken(
    override val tokenType: RegexTokenType,
    override val text: String,
    override val startOffset: Int,
    override val endOffset: Int,
) extends Token(tokenType, startOffset, endOffset)