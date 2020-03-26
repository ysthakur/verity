package com.ysthakur.parsing.parser

import com.ysthakur.parsing.LexerOrParserHelper
import com.ysthakur.parsing.lexer.Token

import scala.collection.mutable.ListBuffer

class ParserHelper extends LexerOrParserHelper[Iterable[Token], Token, Node, Iterable[Token]](ParserDef) {
    override def process(): Node = ???

    /**
     * Get the next character (if this is a lexer) or token (if this is a parser).
     *
     * @return The next piece of input or null if it's reached the end of the file
     *         or input stream
     */
    override def getNext: Token = ???

    /**
     * Whether or not it can proceed
     *
     * @return
     */
    override def hasNext: Boolean = ???

    /**
     * Add this piece of the input (character/token) to the
     * accumulator, which is a StringBuilder or something
     */
    override def accumulate(input: Token): Unit = ???

    /**
     * Return the next character/token without consuming it.
     *
     * @return
     */
    override def peekNext: Token = ???

    override def emptyAccumulator(): Iterable[Token] = ???

    override def accumulate(acc: Iterable[Token], input: Token): Unit = ???
}
