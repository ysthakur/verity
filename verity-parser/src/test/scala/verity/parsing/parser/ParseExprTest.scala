package verity.parsing.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing._, ast._, parser._
import verity.parsing.parser._

class ParseExprTest extends AnyFlatSpec {
  def test() = {
    
    import fastparse._
    val file = new File(
        raw"C:\Users\yasht\verity\verity-parser\src\test\resources\exprtest"
    )
    val fis = new FileInputStream(file)
    parse(fis, Exprs.expr(_)) match {
      case Parsed.Success(matched, _) => matched
    }
  }

  val matched = test().text
  val shouldEqual = """((x.foo(("bar" + 3))[4].baz ^ ((6 - (xyzzy[(5 + 944)][baz][7] * 45)) + (((this) / 45) * 99))) || (true && false))"""
  println(matched)
  matched should s"""equal $shouldEqual""" in {
    assert(matched.replaceAll("\\s", "") == 
      shouldEqual.replaceAll("\\s", ""))
  }
}
