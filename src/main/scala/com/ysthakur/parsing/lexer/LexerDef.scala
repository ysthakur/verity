package com.ysthakur.parsing.lexer

import java.io.InputStream

import com.ysthakur.parsing.{LexerOrParser, Pattern, PatternCase, StateCase}

/**
 * Originally intended to a definition of the lexer with the DSL
 */
object LexerDef extends LexerOrParser[Char, Iterable[Token], StringBuilder]() with Dynamic {

    override type InputSource = InputStream
    override type Helper = LexerHelper

    private val action: This#Helper => Unit = (helper: This#Helper) => {
        val lastTokenType =
            patternCaseToTokenType(helper.lastMatch._1.asInstanceOf[PatternCase[Char, Helper]])
        helper.asInstanceOf[Helper].lastToken =
            lastTokenType match {
                case textTokenType: FixedTextTokenType =>
                    new InvariantToken(textTokenType, helper.offset)
                case regexTokenType: RegexTokenType =>
                    new VariantToken(regexTokenType, helper.lastMatch._2.toString, helper.offset)
            }
    }

    private def makePattern(tt: TokenType): Pattern[Char] = {
        tt match {
            case rtt: RegexTokenType => RegexPattern(rtt.regex)
            case ftt: FixedTextTokenType => FixedTextPattern(ftt.text)
            case _ => throw new Error("Unexpected type")
        }
    }

    private val patternCaseToTokenType =
        JMMTokenTypes.allTokenTypes.map[PatternCase[Char, Helper], TokenType] {
            case (_, tokenType) =>
                PatternCase[Char, Helper](makePattern(tokenType), action) -> tokenType
        }

    /*println("Symbol tokens  = " + SymbolTokenTypes.allTokenTypes.values)
    println("Variant types  = " + VariantTextTokenTypes.allTokenTypes.values)
    println("All token types= " + JMMTokenTypes.allTokenTypes.values)
    println("Lexertokentypes= " + patternCaseToTokenType.values)*/

    this.addStateCase(StateCase(defaultState, patternCaseToTokenType.keys))

    override def makeHelper(inputSource: InputStream): LexerHelper = LexerHelper(inputSource)
}
