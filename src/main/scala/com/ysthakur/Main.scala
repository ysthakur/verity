package com.ysthakur

import java.io.{BufferedInputStream, File, FileInputStream}
import java.util.regex.Pattern

import com.ysthakur.parsing.lexer.{JMMTokenTypes, LexerDef, VariantTextTokenTypes}

object Main {

    def main(args: Array[String]): Unit = {
        //regex()
        lex()
    }

    def regex(): Unit = {
        val pattern = Pattern.compile(s"${VariantTextTokenTypes.MULTILINE_COMMENT.regex}$$")
        val matcher = pattern.matcher("/* This is a simple Java program.   FileName : \"HelloWorld.java\". */")
        println(matcher.matches())
        println(matcher.requireEnd())
    }

    def lex(): Unit = {
        val file = new File("C:\\Users\\thaku\\Ideaprojects\\" +
            "javamm-scala\\src\\test\\resources\\lexertest.java")
        val tokens = LexerDef.process(new BufferedInputStream(new FileInputStream(file))).toList
        println(tokens)
        println(tokens.map(token => {
            val tt = token.tokenType
            JMMTokenTypes.allTokenTypes.keys.find {name =>
                JMMTokenTypes.allTokenTypes(name) == tt
            }
        }))
    }

}
