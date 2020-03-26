package com.ysthakur.parsing.parser

import com.ysthakur.parsing.LexerOrParser
import com.ysthakur.parsing.lexer.Token

/**
 * TODO implement this
 */
object ParserDef extends LexerOrParser[Token, Node, Iterable[Token]]() {
    override type InputSource = Iterable[Token]
    override type Helper = ParserHelper

    override def makeHelper(inputSource: Iterable[Token]): ParserHelper = ???
}
