package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

import cats.data.NonEmptyList

import org.scalatest.funsuite.AnyFunSuite

class FullTests extends VerityParserTestSuite {
  test("Simple module with imports, types, and vars") {
    testResult(
      SourceModule(
        "Foo",
        List(
          ImportStmt(NonEmptyList("foo", List("bar"))),
          ImportStmt(NonEmptyList("foo", List("blech", "xyzzy", "baz")))
        ),
        List(
          SourceModule(
            "Submodule",
            Nil,
            Nil,
            Nil,
            List(
              GlobalVar(
                "something",
                ToBeInferred,
                LetExpr(
                  List(
                    LocalVar(
                      "foobar",
                      UnresolvedType(List("String")),
                      IntLiteral(5)
                    )
                  ),
                  VarRef("x", null, Span.synthetic)
                )
              )
            )
          )
        ),
        Nil,
        Nil
      )
    )(
      Parser.parseModule(
        "test",
        """
        mod Foo
          import foo.bar
          import blech.xyzzy.baz

          record Foo[A](field1: Int, field2: Map[String, Option[String]])

          let something = let x = 5 in x

          mod Submodule
            let foobar: String = 3
          end
        end
        """
      )
    )
  }
}
