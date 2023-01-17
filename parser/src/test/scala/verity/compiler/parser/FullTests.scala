package verity.compiler.parser

import org.scalatest.funsuite.AnyFunSuite

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

class FullTests extends AnyFunSuite {
  test("Module should parse properly") {
    assertResult(
      
    )(Parser.parseString("test", """
    """))
  }
}
