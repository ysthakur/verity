package com.ysthakur

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer.{JMMTokenTypes, Lexer, Token, VariantTextTokenTypes}
import com.ysthakur.parsing.parser.Parser

object Main {

  def main(args: Array[String]): Unit = {
      //regex()
      val tokens = lex()
      val ast = parse(tokens)
      println("AST = " + ast)
  }

  def parse(tokens: Iterable[Token]): Node = {
    Parser.createAST(tokens)
  }

  def lex(): Iterable[Token] = {
    val file = new File(
        "C:\\Users\\thaku\\Ideaprojects\\" +
          "javamm-scala\\src\\test\\resources\\lexertest.java")
    val tokens =
      Lexer.process(new BufferedInputStream(new FileInputStream(file))).toList
    println(tokens)
    println(tokens.map(token => {
      val tt = token.tokenType
      JMMTokenTypes.allTokenTypes.keys.find { name =>
        JMMTokenTypes.allTokenTypes(name) == tt
      }
    }))
    tokens
  }

  def regex(): Unit = {
    val pattern =
      Pattern.compile(s"${VariantTextTokenTypes.MULTILINE_COMMENT.regex}$$")
    val matcher = pattern.matcher(
        "/* This is a simple Java program.   FileName : \"HelloWorld.java\". */")
    println(matcher.matches())
    println(matcher.requireEnd())
  }

}

/*
//regex()
    //val tokens = lex()
    //val ast = parse(tokens)
    /**
      * Keywords that cannot be used as valid identifiers
      */
    val reservedKeywords = List(
        "import",
        "package",
        "public",
        "private",
        "protected",
        "super",
        "default",
        "extends",
        "static",
        "abstract",
        "final",
        "native",
        "transient",
        "volatile",
        "synchronized",
        "const",
        "switch",
        "case",
        "while",
        "for",
        "if",
        "else",
        "var",
        "class",
        "trait",
        "enum",
        "where",
        "throws",
        "implies",
        "assert",
        "new",
        "continue",
        "break",
        "throw",
        "return",
        "as",
        "is",
        "null",
        "true",
        "false",
        "with",
        "bool",
        "byte",
        "short",
        "int",
        "char",
        "float",
        "double",
        "long",
        "void"
    )
    val f = new File(new File("").getAbsolutePath + "\\output")
    f.createNewFile()
    val writer = new FileWriter(f)
    reservedKeywords.foreach(
        word =>
          writer.write(s"""val ${word.toUpperCase}: KTT = make("$word")\n""")
    )
    writer.close()
 */
