import org.scalatest.funsuite.AnyFunSuite

import verity.ast.*
import verity.parser.Exprs

class ParserTests extends AnyFunSuite {
  test("Basic arithmetic and stuff") {
    assert(
      Exprs.expr.parse("3 + 2 * 6 > abc == true") ==
        Right(
          "" ->
            BinExpr(
              BinExpr(
                BinExpr(
                  IntLiteral(3, TextRange(0, 1)),
                  Op("+", TextRange(2, 3)),
                  BinExpr(
                    IntLiteral(2, TextRange(4, 5)),
                    Op("*", TextRange(6, 7)),
                    IntLiteral(6, TextRange(8, 9))
                  )
                ),
                Op(">", TextRange(10, 11)),
                UnresolvedIdentifier("abc", TextRange(12, 15))
              ),
              Op("==", TextRange(16, 18)),
              BoolLiteral(true, TextRange(19, 23))
            )
        )
    )
  }
}
