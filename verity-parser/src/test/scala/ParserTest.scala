import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

import verity.parsing._, ast._, parser._

class ParserTest extends AnyFlatSpec {
  def parse() = {
    val file = new File(
        raw"C:\Users\yasht\verity\verity-parser\src\test\resources\parsertest"
    )
    val reader = new Reader(file)
    val ast = Parser.parse(reader)
    ast
  }

  val parsed = parse()
  println(parsed.text)
  assert(parsed.text == "true - false + true")
}