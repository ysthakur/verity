import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing.lexer.{Token, Lexer}
// import verity.parsing.ast.INode
import verity.parsing.parser._

object Main /*extends App*/ {

  def main(args: Array[String]): Unit = {
    // val f = new File(new File("").getAbsolutePath + "\\output")
    // f.createNewFile()
    // val writer = new FileWriter(f)
    // //writer.write("package verity.parsing.parser\n\nimport scala.util.parsing.combinator.RegexParsers\n\nclass NewLexer extends RegexParsers {\n")
    // writer.write("")
    // val tri = "\"\"\""
    // for (symbol <- SymbolTokenType.values) {
    //   //if (symbol == null) throw new Error()
    //   writer.write("\nobject " + symbol.name + " extends SymbolTokenType(" + tri + symbol.text + tri + ")")
    // }
    // writer.write("}")
    // writer.close()
    
    val ast = parse()
    //println("AST = " + ast)
    println(s"\nText = ${ast.text}")
  }

  def parse() = {
    val file = new File(
        "C:\\Users\\yasht\\verity\\verity-parser\\src\\test\\resources\\lexertest"
    )
    val reader = new Reader(file)
    val ast = Parser.parse(reader)
    ast
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
