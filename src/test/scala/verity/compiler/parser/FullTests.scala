package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.{Exprs, Types}

import cats.data.{Chain, NonEmptyList, NonEmptyChain}

import org.scalatest.funsuite.AnyFunSuite

class FullTests extends VerityParserTestSuite {
  test("Simple module with imports, types, and vars") {
    testResult(
      SourceModule(
        "Foo",
        List(
          ImportStmt(NonEmptyChain("foo", "bar")),
          ImportStmt(NonEmptyChain("blech", "xyzzy", "baz"))
        ),
        List(
          SourceModule(
            "Submodule",
            Nil,
            Nil,
            Nil,
            List(GlobalVar("foobar", UnresolvedType("String"), IntLiteral(3)))
          )
        ),
        List(
          Record(
            "Foo",
            ComptimeParams(List(TypeParam("A")), Nil, Nil),
            Params(
              List(
                Param("field1", UnresolvedType("Int")),
                Param(
                  "field2",
                  TypeApply(
                    UnresolvedType("Map"),
                    List(
                      UnresolvedType("String"),
                      TypeApply(
                        UnresolvedType("Option"),
                        List(UnresolvedType("String"))
                      )
                    )
                  )
                )
              ),
              Nil
            )
          )
        ),
        List(
          GlobalVar(
            "something",
            ToBeInferred,
            LetExpr(
              List(LocalVar("x", ToBeInferred, IntLiteral(5))),
              UnresolvedIdentifier("x")
            )
          )
        )
      )
    )(
      Parser.parseModule(
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
