package verity.parsing.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

// import verity.parsing._, ast._, parser._
import verity.parsing.parser._

class ParserTest extends AnyFlatSpec {
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
  println(matched)
  matched should "equal ( ( 92 ^ ( ( 6 - ( 8 * 39 ) ) > ( 45 + ( ( (this) / 45 ) * 99 ) ) ) ) || ( true && ( false | 3 ) ) )" in {
    assert(matched.replaceAll("\\s", "") == "( ( 92 ^ ( ( 6 - ( 8 * 39 ) ) > ( 45 + ( ( (this) / 45 ) * 99 ) ) ) ) || ( true && ( false | 3 ) ) )".replaceAll("\\s", ""))
  }
}
