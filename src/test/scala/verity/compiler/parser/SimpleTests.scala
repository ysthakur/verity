package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

import cats.data.NonEmptyList

import org.scalatest.funsuite.AnyFunSuite

class ParserTests extends AnyFunSuite {
  test("Empty module should parse properly") {
    assertResult(
      SourceModule("Test", Nil, Nil, Nil, Nil)
    )(
      Parser.parseModuleContents("Test.vt", "Test", "").value.get
    )
  }

  test("Basic arithmetic and stuff") {
    assert(
      Exprs.expr.parse("3 + 2 * 6 > abc == true") ===
        Right(
          "" ->
            BinExpr(
              BinExpr(
                BinExpr(
                  IntLiteral(3),
                  Op("+"),
                  BinExpr(
                    IntLiteral(2),
                    Op("*"),
                    IntLiteral(6)
                  )
                ),
                Op(">"),
                UnresolvedIdentifier("abc")
              ),
              Op("=="),
              BoolLiteral(true)
            )
        )
    )
  }

  test("Basic val expression without type") {
    assertResult(
      Right(
        LetExpr(
          List(LocalVar("x", ToBeInferred, IntLiteral(5))),
          IntLiteral(3)
        )
      )
    )(Exprs.expr.parseAll("let x = 5 in 3"))
  }

  test("Basic let expression with type") {
    assertResult(
      Right(
        LetExpr(
          List(
            LocalVar(
              "x",
              UnresolvedType("verity", "lang", "Int"),
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
          UnresolvedType("verity", "lang", "Int"),
          List(
            UnresolvedType("A"),
            TypeApply(
              TypeMemberAccess(
                TypeApply(
                  UnresolvedType("B"),
                  List(UnresolvedType("D"))
                ),
                "Foo"
              ),
              List(UnresolvedType("C"))
            )
          )
        )
      )
    )(Types.typ.parseAll("verity.lang.Int[A, B[D].Foo[C]]"))
  }
}
