package verity.ast

import verity.ast.*

trait TypeDef extends NamedTree {
  def typeParams: List[TypeParam] = Nil
  def proofParams: List[ValParam] = Nil
}

object TypeDef {
  val placeholder = new TypeDef {}
}

object BuiltinTypes {
  case class Void() extends TypeDef
  case class Boolean() extends TypeDef
  case class Number() extends TypeDef
  case class Char() extends TypeDef
  case class String() extends TypeDef
}

object NotGivenDef extends TypeDef

object NotProvenDef extends TypeDef

class TypeParam(val name: String) extends NamedTree, TypeDef

case class TypeAlias(override val typeParams: List[TypeParam], override val proofParams: List[ValParam]) extends TypeDef
