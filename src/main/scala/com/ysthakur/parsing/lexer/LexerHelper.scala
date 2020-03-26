package com.ysthakur.parsing.lexer

import java.io.{IOException, InputStream}

import com.ysthakur.parsing.LexerOrParserHelper

import scala.collection.mutable.ListBuffer

case class LexerHelper(file: InputStream)
    extends LexerOrParserHelper[InputStream, Char, Iterable[Token], StringBuilder](LexerDef) {
    private[lexer] val tokens = ListBuffer[Token]()
    private[lexer] var lastToken: Token = _

    private var passedRows, passedCols = 0

    override def process(): Iterable[Token] = {
        run()
        tokens
    }

    override def update(): Unit = {
        if (!lastToken.isInstanceOf[IgnoredTokenType]) tokens += lastToken
        val text = lastToken.text
        val nRows = text.count(_ == '\n')
        passedRows += nRows
        val len = text.length
        //todo check if this is correct
        if (nRows > 0) passedCols = len - text.lastIndexOf('\n') - 2
        else passedCols += len
    }

    @throws[IOException]
    override def getNext: Char = {
        file.read().toChar
    }

    override def hasNext: Boolean = {
        try {
            val next = peekNext
            next != -1
        } catch {
            case e: IOException => false
        }
    }

    override def accumulate(acc: StringBuilder, input: Char): Unit = acc.addOne(input)

    override def emptyAccumulator(): StringBuilder = new StringBuilder()

    @throws[IOException]
    override def peekNext: Char = {
        file.mark(4)
        val res = file.read()
        file.reset()
        res.toChar
    }

    override def end(): Unit = {
        file.close()
    }

    override def getPosition: String = s"($passedRows,$passedCols), offset=$offset"
}