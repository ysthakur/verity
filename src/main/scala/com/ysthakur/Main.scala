package com.ysthakur

import java.io.{BufferedInputStream, File, FileInputStream}
import java.util.regex.Pattern

import com.ysthakur.parsing.lexer.{LexerDef, VariantTextTokenTypes}

object Main {

    def main(args: Array[String]): Unit = {
        lex()
    }

    def regex(): Unit = {
        val pattern = Pattern.compile(s"^${VariantTextTokenTypes.VALID_ID.regex}$$")
        val matcher = pattern.matcher("H")
        println(matcher.matches())
        println(matcher.requireEnd())
    }

    def lex(): Unit = {
        val file = new File("C:\\Users\\thaku\\Ideaprojects\\" +
            "javamm-scala\\src\\test\\resources\\lexertest.java")
        val tokens = LexerDef.process(new BufferedInputStream(new FileInputStream(file))).toList
        println(tokens)
    }

}
