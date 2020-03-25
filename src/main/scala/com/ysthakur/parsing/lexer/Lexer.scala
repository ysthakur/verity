package com.ysthakur.parsing.lexer

import java.io.FileInputStream

import com.ysthakur.parsing.dsl._
import com.ysthakur.parsing.{FullMatch, MatchResult, NoMatch}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class Lexer(val file: FileInputStream) extends
    LexerOrParser[Char, Token, StringBuilder](new StringBuilder(), "INITIAL")
    with Dynamic {

    private var lastTokenType: TokenType = _
    private val tokens = ListBuffer[Token]()

    s.INITIAL := (
        PredicatePattern[Char](input => {
            val text = toStr(input)
            var res: MatchResult = NoMatch()
            breakable {
                for (tokenType <- (FixedTextTokenTypes.allTokenTypes
                    ++ KeywordTokenTypes.allTokenTypes).values) {
                    if (text == tokenType.text) res = FullMatch(false)
                }
            }
            res
        }) --> {
            tokens.addOne(
                if (lastTokenType.isInstanceOf[FixedTextTokenType]) InvariantToken(lastTokenType)
                else VariantToken(lastTokenType, current)
            )
        }
    )

    override def getNext: Char = file.read().toChar

    override def accumulate(input: Char): Unit = current.addOne(input)

    override def peekNext: Char = {
        file.mark(2)
        val res = file.read()
        file.reset()
        res.toChar
    }
}
