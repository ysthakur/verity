package com.ysthakur.parsing.lexer

abstract class Token(val tokenType: TokenType)

/**
 * A token whose text is always the same
 * @param tokenType
 */
case class InvariantToken(override val tokenType: TokenType) extends Token(tokenType)

/**
 * A token whose text may be different, like a string literal
 * @param tokenType
 * @param text
 */
case class VariantToken(override val tokenType: TokenType, text: CharSequence) extends Token(tokenType)