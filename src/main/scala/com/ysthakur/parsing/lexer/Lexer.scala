package com.ysthakur.parsing.lexer

import java.io.InputStream

import com.ysthakur.parsing.{LexerOrParser, Pattern, PatternCase, StateCase}

object Lexer extends LexerOrParser[Char, Iterable[Token], StringBuilder]() with Dynamic {

    override type InputSource = InputStream
    override type Helper = LexerHelper

    private val action: This#Helper => Unit = (helper: This#Helper) => {
        val lastTokenType = patternCaseToTokenType(helper.lastMatch.asInstanceOf[PatternCase[Char, Helper]])
        if (!lastTokenType.isInstanceOf[IgnoredTokenType])
            helper.asInstanceOf[Helper].tokens.addOne(
                if (lastTokenType.isInstanceOf[FixedTextTokenType]) InvariantToken(lastTokenType)
                else VariantToken(lastTokenType, helper.current.asInstanceOf))
    }

    private def makePattern(tt: TokenType): Pattern[Char] = {
        tt match {
            case rtt: RegexTokenType => RegexPattern(rtt.regex)
            case ftt: FixedTextTokenType => FixedTextPattern(ftt.text)
            case _ => throw new Error("Unexpected type")
        }
    }

    private val patternCaseToTokenType = JMMTokenTypes.allTokenTypes.map {
            case (_, tokenType) => PatternCase[Char, Helper](makePattern(tokenType).asInstanceOf,
                    action) -> tokenType
    }

    this.addStateCase(StateCase(defaultState, patternCaseToTokenType.keys))

    override def makeHelper(inputSource: InputStream): LexerHelper = LexerHelper(inputSource)
}
