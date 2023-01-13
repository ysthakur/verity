

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
              Prop(
                "foo",
                UnresolvedType(List("int")),
                Some(IntLiteral(0, TextRange.synthetic))
              ),
              Prop(
                "x",
                UnresolvedType(List("string")),
                None
              ),
              Prop(
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
      TypeDefs.classOrTrait.parse(
        """class Foo
             val foo: int = 0
             val x: string
             val y = 8"""
      )
    }
  }
}
