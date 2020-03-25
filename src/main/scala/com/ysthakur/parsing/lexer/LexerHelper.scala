package com.ysthakur.parsing.lexer

import java.io.{IOException, InputStream}

import com.ysthakur.parsing.LexerOrParserHelper

import scala.collection.mutable.ListBuffer

case class LexerHelper(file: InputStream)
    extends LexerOrParserHelper[InputStream, Char, Iterable[Token], StringBuilder](Lexer, new StringBuilder) {

    private[lexer] val tokens = ListBuffer[Token]()

    override def process(): Iterable[Token] = {
        run()
        tokens
    }

    @throws[IOException]
    override def getNext: Char = file.read().toChar

    override def hasNext: Boolean = {
        try {
            val next = peekNext
            next != -1
        } catch {
            case e: IOException => false
        }
    }

    override def accumulate(input: Char): Unit = current.addOne(input)

    @throws[IOException]
    override def peekNext: Char = {
        file.mark(2)
        val res = file.read()
        file.reset()
        res.toChar
    }

    override def end(): Unit = {
        file.close()
    }
}