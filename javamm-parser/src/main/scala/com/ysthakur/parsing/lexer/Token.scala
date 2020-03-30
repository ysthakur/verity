package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.grammar.{ExactMatch, Match}

abstract class Token(val tokenType: TokenType) extends Node with HasText {}

/**
  * A token whose text is always the same
  *
  * @param tokenType
  */
case class InvariantToken(
    override val tokenType: FixedTextTokenType,
    startOffset: Int,
    endOffset: Int
) extends Token(tokenType) {

  /**
    * Construct an [[InvariantToken]] knowing the token type and the start offset. The
    * endOffset is known because of the length of the text, which is known because tokens
    * of the given token type always have the same text.
    *
    * @param tokenType
    * @param startOffset
    */
  def this(tokenType: FixedTextTokenType, startOffset: Int) =
    this(tokenType, startOffset, startOffset + tokenType.text.length)

  override def unapply(): (CharSequence, Int, Int) =
    (text, startOffset, endOffset)

  override def text: String = tokenType.text
}

/**
  * A token whose text may be different, like a string literal
  *
  * @param tokenType
  */
case class VariantToken(
    override val tokenType: RegexTokenType,
    matched: Match[Char]
) extends Token(tokenType) {

  /**
    * Utility constructor to make tokens knowing only the start offset, since the end offset
    * can be obtained by adding the start offset to the length of the text that this token
    * contains.
    *
    * @param tokenType
    * @param text
    * @param startOffset
    * @return
    */
  def this(tokenType: RegexTokenType, text: String, startOffset: Int) =
    this(tokenType, ExactMatch(text, startOffset, startOffset + text.length))

  override def unapply(): (CharSequence, Int, Int) =
    (text, matched.start, matched.end)

  override def text: String = matched.asInstanceOf[HasText].text
}
