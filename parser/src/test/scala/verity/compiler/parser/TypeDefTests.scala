package verity.compiler.parser

import org.scalatest.funsuite.AnyFunSuite

import verity.compiler.ast.*

class TypeDefTests extends AnyFunSuite {
  test("Basic record def") {
    assertResult(
      Right(
        (
          "",
          Record(
            "Foo",
            ComptimeParams(
              List(
                TypeParam("A"),
                TypeParam("B")
              ),
              Nil,
              Nil
            ),
            Params(
              List(
                Param("foo", UnresolvedType(List("Int"))),
                Param("x", TypeApply(UnresolvedType(List("A")), List(UnresolvedType(List("C"))))),
                Param("y", UnresolvedType(List("B")))
              ),
              List(
                Param("foo", UnresolvedType(List("Boolean")))
              )
            )
          )
        )
      )
    ) {
      TypeDefs.typeDef.parse(
        """record Foo[A, B] (foo: Int, x: A[C], y: B)(given foo: Boolean)"""
      )
    }
  }
}
