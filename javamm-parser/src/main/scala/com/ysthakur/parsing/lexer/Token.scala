package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.{ExactMatch, HasText, Match}

/**
  *
  * @param tokenType The type of this token
  * @param startOffset The index in the file where this token starts
  * @param endOffset The index in the file <em>before</em> which this token ends
  */
sealed abstract class Token[+T <: TokenType](
    val tokenType: T,
    val startOffset: Int,
    val endOffset: Int
) extends Node
    with HasText {
  def text: String
}

object Token {
  def isValidId(token: Token[?]): Boolean = 
    token.tokenType.isInstanceOf[ValidIdentifierTokenType]
  def unapply[T <: TokenType](arg: Token[T]): Option[(T, Int, Int)] = 
    Some(arg.tokenType, arg.startOffset, arg.endOffset)
}

/**
  * A token whose text is always the same
  *
  * @param tokenType
  */
case class InvariantToken[+F <: FixedTextTokenType](
    override val tokenType: F,
    override val startOffset: Int = -1,
    override val endOffset: Int = -1
) extends Token[F](tokenType, startOffset, endOffset) {
  override val text: String = tokenType.text
}

/**
  * A token whose text may be different, like a string literal
  *
  * @param tokenType
  */
case class VariantToken[+R <: RegexTokenType](
    override val tokenType: R,
    override val text: String,
    override val startOffset: Int,
    override val endOffset: Int
) extends Token[R](tokenType, startOffset, endOffset)
