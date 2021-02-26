import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing.lexer.{Token, Lexer}
// import verity.parsing.ast.INode
import verity.parsing.parser._

object ParserTestMain {
  def main(args: Array[String]): Unit =
    new ParserTest()
}