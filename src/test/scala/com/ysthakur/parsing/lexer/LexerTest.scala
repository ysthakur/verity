package com.ysthakur.parsing.lexer

import java.io.{File, FileInputStream}

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class LexerTest extends AnyFlatSpec with Matchers {

    def lex(): Unit = {
        val file = new File("C:\\Users\\thaku\\Ideaprojects\\" +
            "javamm-scala\\src\\test\\resources\\lexertest.java")
        val tokens = Lexer.process(new FileInputStream(file))
        println(tokens)
    }

    "Lexer" should "turn all of that stuff into tokens" in {
        lex()
    }

}
