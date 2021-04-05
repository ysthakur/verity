package verity.ast.infile

import verity.ast._
import verity.parsing._

sealed trait Type extends Tree, HasText {
  def strictSubTypeOf(sup: Type): Boolean
  def strictSuperTypeOf(sub: Type): Boolean
}

object Type {

}

object AnyType extends Type with Synthetic {
  def strictSubTypeOf(sup: Type) = false
  def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Any"
}

object ObjectType extends Type, Synthetic {
  def strictSubTypeOf(sup: Type) = false
  def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Object"
}

object NothingType extends Type, Synthetic {
  def strictSubTypeOf(sup: Type) = false
  def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Nothing"
}

enum Primitive extends Type {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

  def strictSubTypeOf(sup: Type): Boolean = ???
  def strictSuperTypeOf(sub: Type) = ???

  def text = this.toString.toLowerCase.nn
  def textRange = TextRange.synthetic
}


case class TypeRef(name: Name, args: Seq[Type]) extends Type {
  def strictSubTypeOf(sup: Type): Boolean = ???
  def strictSuperTypeOf(sub: Type) = ???

  def text = if (args.isEmpty) name.text else s"${name.text}<${args.mkString(",")}>"
  override def textRange: TextRange = if (args.isEmpty) name.textRange else TextRange(name.textRange.start, args.last.textRange.end)

  private var _resolve: Option[TypeDef] = None
  def resolve: Option[TypeDef] = _resolve
  private[verity] def resolve_=(typeDef: TypeDef) =
    _resolve = Some(typeDef)
}

class TypeRange(var upper: Type, var lower: Type) extends Type {
  def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)

  def text = ???

  override def textRange: TextRange = ???
}

case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
  def strictSubTypeOf(sup: Type): Boolean = typ.strictSubTypeOf(sup)
  def strictSuperTypeOf(sub: Type) = typ.strictSuperTypeOf(sub)

  def text = s"(${typ.text})"
}

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  def strictSuperTypeOf(sub: Type) = lower.fold(false)(_.strictSuperTypeOf(sub))

  def text = ???

  override def textRange: TextRange = ???
}

//TODO arrays
class ArrayType

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)
  
  def text = ???

  override def textRange: TextRange = ???
}