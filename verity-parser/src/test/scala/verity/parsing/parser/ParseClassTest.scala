package verity.parsing.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing._, ast._, parser._
// import verity.parsing.parser._

import fastparse.Parsed

class ParseClassTest extends AnyFlatSpec {
  def parse() = {
    val file = new File(
        raw"C:\Users\yasht\verity\verity-parser\src\test\resources\classtest"
    )
    val fis = new FileInputStream(file)
    val ast = Parser.parseFile(fis)
    ast
  }

  val Parsed.Success(matched, index) = parse()
  println(matched.text)
  matched.text should "be parsed properly" in {
    assert(matched.text.replaceAll("\\s", "") == 
    """
    class Foo {
      public static void main () {
        (1 + 2);
      }
    }""".stripMargin.replaceAll("\\s|\n", ""))
  }
}