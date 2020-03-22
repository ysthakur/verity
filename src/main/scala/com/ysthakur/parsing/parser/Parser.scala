package com.ysthakur.parsing.parser

import com.ysthakur.parsing.dsl.Tokenizer
import com.ysthakur.parsing.lexer.Token

/**
 * TODO implement this
 */
class Parser extends Tokenizer[Token, Node, Iterable[Node]](null) {
    /**
     * Get the next character (if this is a lexer) or token (if this is a parser)
     *
     * @return
     */
    override def getNext: Token = ???

    /**
     * Add this piece of the input (character/token) to the
     * accumulator, which is a StringBuilder or something
     */
    override def accumulate(input: Token): Unit = ???

    /**
     * Return the next character/token without consuming it
     *
     * @return
     */
    override def peekNext: Token = ???
}
