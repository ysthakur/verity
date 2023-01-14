

import org.scalatest.funsuite.AnyFunSuite

import verity.compiler.ast.*

class TypeDefTests extends AnyFunSuite {
  test("Basic class def") {
    assertResult(
      Right(
        (
          "",
          TypeDef(
            "Foo",
            List.empty,
            List(
              Field(
                "foo",
                UnresolvedType(List("int")),
                Some(IntLiteral(0, TextRange.synthetic))
              ),
              Field(
                "x",
                UnresolvedType(List("string")),
                None
              ),
              Field(
                "y",
                UnknownType,
                Some(IntLiteral(8, TextRange.synthetic))
              )
            ),
            false
          )
        )
      )
    ) {
      TypeDefs.typedef.parse(
        """record Foo(foo: int, x: string, y: int)"""
      )
    }
  }
}
