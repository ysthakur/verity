import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing.lexer.{Token, Lexer}
// import verity.parsing.ast.INode
import verity.parsing.parser._

object ParserTestMain:
  def main(args: Array[String]): Unit =
    val file = new File(
      raw"C:\Users\yasht\verity\verity-parser\src\test\resources\parsertest"
    )
    val reader = new Reader(file)
    val ast = Parser.parse(reader)
    println(ast.text)
    assert(ast.text == "true")