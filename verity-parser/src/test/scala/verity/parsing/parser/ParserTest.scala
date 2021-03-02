package verity.parsing.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing._, ast._, parser._
import verity.parsing.parser._

class ParserTest extends AnyFlatSpec {
  def parse() = {
    val file = new File(
        raw"C:\Users\yasht\verity\verity-parser\src\test\resources\parsertest"
    )
    val fis = new FileInputStream(file)
    val ast = Parser.parseFile(fis)
    ast
  }

  val parsed = parse()
  // println(parsed.text)
  // assert(parsed.text == "true - false + true")
}