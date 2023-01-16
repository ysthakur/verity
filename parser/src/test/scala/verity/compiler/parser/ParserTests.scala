package verity.compiler.parser

import org.scalatest.funsuite.AnyFunSuite

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

class ParserTests extends AnyFunSuite {
  test("Basic arithmetic and stuff") {
    assert(
      Exprs.expr.parse("3 + 2 * 6 > abc == true") ===
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

  test("Basic val expression without type") {
    assertResult(
      Right(
        LetExpr(
          List(VarDef("x", None, IntLiteral(5, TextRange(8, 9)))),
          IntLiteral(3, TextRange(13, 14))
        )
      )
    )(Exprs.expr.parseAll("let x = 5 in 3"))
  }

  test("Basic let expression with type") {
    assertResult(
      Right(
        LetExpr(
          List(
            VarDef(
              "x",
              Some(UnresolvedType(List("verity", "lang", "Int"))),
              IntLiteral(5)
            )
          ),
          IntLiteral(3)
        )
      )
    )(Exprs.expr.parseAll("let x: verity.lang.Int = 5 in 3"))
  }

  test("Somewhat complex type") {
    assertResult(
      Right(
        TypeApply(
          UnresolvedType(List("verity", "lang", "Int")),
          List(
            UnresolvedType(List("A")),
            TypeApply(
              TypeMemberAccess(
                TypeApply(
                  UnresolvedType(List("B")),
                  List(UnresolvedType(List("D")))
                ),
                "Foo"
              ),
              List(UnresolvedType(List("C")))
            )
          )
        )
      )
    )(Types.typ.parseAll("verity.lang.Int[A, B[D].Foo[C]]"))
  }
}
