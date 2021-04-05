package verity.parsing.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertions._

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter}
import java.util.regex.Pattern

import verity.parsing.GetText
import GetText._
import verity.ast._

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
    assert(matched.text.replaceAll("\\s|\n", "") == 
    """
    package foo.bar.baz;
    import java.util.ArrayList;
    import java.util.*;
    class Foo {
      public static void main () {
        System.out.println("Hello world!");
      }
      private static void foo(int bar);
    }""".stripMargin.replaceAll("\\s|\n", ""))
  }
}
