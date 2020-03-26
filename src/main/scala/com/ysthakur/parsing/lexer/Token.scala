package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.TextRange
import com.ysthakur.parsing.parser.Node

abstract class Token(val tokenType: TokenType, val text: String, val textRange: TextRange) extends Node

/**
 * A token whose text is always the same
 * @param tokenType
 */
case class InvariantToken(override val tokenType: FixedTextTokenType,
                          override val textRange: TextRange) extends Token(tokenType, tokenType.text, textRange) {
    /**
     * Construct an [[InvariantToken]] knowing the token type and the start offset. The
     * endOffset is known because of the length of the text, which is known because tokens
     * of the given token type always have the same text.
     * @param tokenType
     * @param startOffset
     */
    def this(tokenType: FixedTextTokenType, startOffset: Int) =
        this(tokenType, TextRange(startOffset, startOffset + tokenType.text.length))
}

/**
 * A token whose text may be different, like a string literal
 * @param tokenType
 * @param text
 */
case class VariantToken(override val tokenType: RegexTokenType,
                        override val text: String,
                        override val textRange: TextRange) extends Token(tokenType, text, textRange) {
    /**
     * Utility constructor to make tokens knowing only the start offset, since the end offset
     * can be obtained by adding the start offset to the length of the text that this token
     * contains.
     * @param tokenType
     * @param text
     * @param startOffset
     * @return
     */
    def this(tokenType: RegexTokenType, text: String, startOffset: Int) =
        this(tokenType, text, TextRange(startOffset, startOffset + text.length))
}