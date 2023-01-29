package verity.compiler.parser

import org.scalatest.funsuite.AnyFunSuite

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

class FullTests extends AnyFunSuite {
  test("Empty module should parse properly") {
    assertResult(
      SourceModule("test", Nil)
    )(
      Parser.parseModule(
        "test",
        "test",
        """
        """
      ).value.get
    )
  }
}
