package com.ysthakur.parsing.lexer

import java.io.FileInputStream

import com.ysthakur.parsing.dsl._
import com.ysthakur.parsing.lexer.RegexTokenTypes._
import com.ysthakur.parsing.lexer.ComplexTokenTypes._

class Lexer(val file: FileInputStream) extends
    Tokenizer[Char, Token, StringBuilder](new StringBuilder(), "INITIAL")
    with Dynamic {

    override def getNext: Char = file.read().toChar

    override def accumulate(input: Char): Unit = {
        current.addOne(input)
    }

    override def peekNext: Char = {
        file.mark(2)
        val res = file.read()
        file.reset()
        res.toChar
    }

    val wsp = "\\W+"

    s.INITIAL := (
        wsp --> {WSP}

    )
}
