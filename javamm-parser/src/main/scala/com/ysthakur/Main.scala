package com.ysthakur

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer.{JMMTokenTypes, Lexer, Token, SymbolTokenType, RegexTokenType, KeywordTokenType}
// import com.ysthakur.parsing.parser.Parser

object Main /*extends App*/ {

  def main(args: Array[String]): Unit = {
    /*val f = new File(new File("").getAbsolutePath + "\\output")
    f.createNewFile()
    val writer = new FileWriter(f)
    //writer.write("package com.ysthakur.parsing.parser\n\nimport scala.util.parsing.combinator.RegexParsers\n\nclass NewLexer extends RegexParsers {\n")
    writer.write("")
    val tri = "\"\"\""
    for (symbol <- SymbolTokenType.values) {
      //if (symbol == null) throw new Error()
      writer.write("\nobject " + symbol.name + " extends SymbolTokenType(" + tri + symbol.text + tri + ")")
    }
    writer.write("}")
    writer.close()*/
    
    //val tokens = lex()
    //val ast    = parse(tokens)
    //println("AST = " + tokens)
  }
  
  def makeDef(name: String|UncheckedNull, body: String): String = {
    new StringBuilder("def ")
        .append(name.toLowerCase)
        .append(": Parser[")
        .append(name.toUpperCase)
        .append("] = {\n    ")
        .append(body)
        .append("\n  }")
        .toString
  }

  // def parse(tokens: Iterable[Token]): Node = {
  //   Parser.createAST(tokens)
  // }

  def lex(): Iterable[Token[?]] = {
    val file = new File(
        "C:\\Users\\thaku\\javamm-scala\\javamm-parser\\src\\test\\resources\\lexertest"
    )
    val lexer = new Lexer(file)
    val tokens = lexer.tokenize().toList
    println(tokens)
    // println(tokens.map(token => {
    //   val tt = token.tokenType
    //   JMMTokenTypes.allTokenTypes.map(p =>
    //   }
    // }))
    lexer.end()
    tokens
  }

  // def regex(): Unit = {
    // val pattern =
    //   Pattern.compile(s"${VariantTextTokenTypes.MULTILINE_COMMENT.regex}$$")
    // val matcher = pattern.matcher(
    //     "/* This is a simple Java program.   FileName : \"HelloWorld.java\". */"
    // )
    // println(matcher.matches())
    // println(matcher.requireEnd())
  // }

}
/*
    val f = new File(new File("").getAbsolutePath + "\\output")
    f.createNewFile()
    val writer = new FileWriter(f)
    reservedKeywords.foreach(
        word =>
          writer.write(s"""val ${word.toUpperCase}: KTT = make("$word")\n""")
    )
    writer.close()
 */
