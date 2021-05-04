package verity.ast.infile

import verity.ast.*

sealed trait Type extends Tree, HasText {

  def subTypeOf(sup: Type): Boolean =
    this == sup || this.strictSubTypeOf(sup)
  def superTypeOf(sub: Type): Boolean =
    this == sub || this.strictSuperTypeOf(sub)
  def strictSubTypeOf(sup: Type): Boolean =
    this != sup && this.subTypeOf(sup)
  def strictSuperTypeOf(sub: Type): Boolean =
    this != sub && this.superTypeOf(sub)
}

object Type {}

object AnyType extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type) = false
  override def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Any"
}

// given ToJava[AnyType.type] = _ => "Type Any"

object ObjectType extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type) = false
  override def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Object"
}

// given ToJava[ObjectType.type] = _ => "Type Object"

object NothingType extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type) = false
  override def strictSuperTypeOf(sub: Type) = sub != AnyType

  def text = "Type Nothing"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

enum PrimitiveType extends Type, Synthetic {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type) = ???

  def text = this.toString.toLowerCase.nn
}
object PrimitiveType {
  val fromName: String => Option[PrimitiveType] =
    PrimitiveType.values.view.map(typ => typ.text -> typ).toMap.get
}

class TypeRef(
    val name: String,
    val args: Iterable[Type],
    val nameRange: TextRange,
    private[this] var resolved: Option[TypeDef] = None
) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type) = ???

  def text = if (args.isEmpty) name else s"$name<${args.mkString(",")}>"
  override def textRange: TextRange =
    if (args.isEmpty) nameRange else TextRange(nameRange.start, args.last.textRange.end)

  def resolve: Option[TypeDef] = resolved
  private[verity] def resolve_=(typeDef: TypeDef) = resolved = Some(typeDef)

  override def equals(other: Any): Boolean = other match {
    case tr: TypeRef => this.name == tr.name && this.args.size == tr.args.size && this.args.lazyZip(tr.args).forall(_ == _)
    case _ => false
  }
}

case class TypeRange(var upper: Type, var lower: Type) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)

  def text = ???

  override def textRange: TextRange = ???
}

case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = typ.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type) = typ.strictSuperTypeOf(sub)

  def text = s"(${typ.text})"
}

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type) = lower.fold(false)(_.strictSuperTypeOf(sub))

  def text = ???

  override def textRange: TextRange = ???
}

//TODO arrays
case class ArrayType()

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)

  def text = "NOT INFERRED AAA!!!"

  override def textRange: TextRange = ???
}

case class TypeParamList(params: Iterable[TypeParam], val textRange: TextRange)
    extends Tree,
      HasText {
  def text: String = HasText.seqText(params, ",", "<", ">")
}

object TypeParamList {
  def empty = TypeParamList(Seq.empty, TextRange.synthetic)
}

class TypeParam(
    val name: String,
    val upperBound: Type,
    val lowerBound: Type,
    val nameRange: TextRange
) extends HasText,
      NamedTree,
      TypeDef {
  def text = s"$name extends ${upperBound.text} super ${lowerBound.text}"
  def textRange = TextRange(
      nameRange.start,
      if (!lowerBound.textRange.isSynthetic) lowerBound.textRange.end
      else if (!upperBound.textRange.isSynthetic) upperBound.textRange.end
      else nameRange.end
  )
}

case class TypeParamBound(boundType: BoundType, typeRepr: TypeRef, textRange: TextRange)
    extends HasText {
  //TODO write this
  def text = ""
}

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}
