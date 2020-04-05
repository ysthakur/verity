package com.ysthakur

import java.io.{BufferedInputStream, File, FileInputStream}
import java.util.regex.Pattern

import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.lexer.{JMMTokenTypes, Lexer, Token}
// import com.ysthakur.parsing.parser.Parser

object Main extends App {

  //def main(args: Array[String]): Unit = {
    //regex()
  val tokens = lex()
  //val ast    = parse(tokens)
  println("AST = " + tokens)
  //}

  // def parse(tokens: Iterable[Token]): Node = {
  //   Parser.createAST(tokens)
  // }

  def lex(): Iterable[Token] = {
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

    /**
 * Keywords that cannot be used as valid identifiers
 */
   /* val reservedKeywords = List(
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
