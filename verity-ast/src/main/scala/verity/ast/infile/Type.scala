package verity.ast.infile

import verity.ast.*

sealed trait Type extends Tree, HasText {

  def fields: Iterable[Field]
  def methods: Iterable[Method]

  // def findField(name: String): Option[Field] = fields.find(_.name == name)
  // def findMethod(name: String): Method

  def subTypeOf(sup: Type): Boolean =
    this == sup || this.strictSubTypeOf(sup)
  def superTypeOf(sub: Type): Boolean =
    this == sub || this.strictSuperTypeOf(sub)
  def strictSubTypeOf(sup: Type): Boolean =
    this != sup && this.subTypeOf(sup)
  def strictSuperTypeOf(sub: Type): Boolean =
    this != sub && this.superTypeOf(sub)
}

object Type {
  val placeholder: Type = new Type {
    override def strictSubTypeOf(sup: Type) = ???
    override def strictSuperTypeOf(sub: Type) = ???

    def fields = Nil
    def methods = ??? //collection.mutable.ListBuffer()

    def text = ???
    def textRange = ???
  }
}

// given ToJava[AnyType.type] = _ => "Type Any"

object BuiltinTypes {
  private[verity] var objectType: Type = Type.placeholder
  private[verity] var stringType: Type = Type.placeholder
}

// given ToJava[ObjectType.type] = _ => "Type Object"

object NothingType extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type) = false
  override def strictSuperTypeOf(sub: Type) = sub != BuiltinTypes.objectType

  def fields = Nil
  def methods = Nil
  override def text = "Type Nothing"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

enum PrimitiveType extends Type, Synthetic {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type) = ???

  def fields = Nil
  def methods = Nil

  override def text = this.toString.toLowerCase.nn
}
object PrimitiveType {
  val fromName: String => Option[PrimitiveType] =
    PrimitiveType.values.view.map(typ => typ.text -> typ).toMap.get

  lazy val numericTypes: TypeUnion = TypeUnion(List(BYTE, SHORT, CHAR, INT, FLOAT, LONG))
}

case class TypeRef(
    val name: Text,
    val args: Iterable[Type],
    private[this] var resolved: Option[TypeDef] = None,
    val argsRange: TextRange
) extends Type {
  def resolve: Option[TypeDef] = resolved
  private[verity] def resolve_=(typeDef: TypeDef) = resolved = Some(typeDef)

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type) = ???

  def fields = resolved.fold(Nil)(_.fields)
  def methods = resolved.fold(Nil)(_.methods)

  override def text = if (args.isEmpty) name.text else s"$name<${args.mkString(",")}>"

  override def textRange: TextRange =
    if (args.isEmpty) name.textRange
    else if (argsRange.isSynthetic) TextRange.synthetic
    else TextRange(name.textRange.start, args.last.textRange.end)

  override def equals(other: Any): Boolean = other match {
    case tr: TypeRef =>
      this.name == tr.name && this.args.size == tr.args.size && this.args
        .lazyZip(tr.args)
        .forall(_ == _)
    case _ => false
  }
}

case class TypeUnion(val types: Iterable[Type]) extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type) = ???

  def fields = ???
  def methods = ???

  def text = ???
}

//todo is this necessary?
case class TypeRange(var upper: Type, var lower: Type) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)

  def fields = upper.fields
  def methods = upper.methods

  override def text = ???
  override def textRange: TextRange = ???
}

// case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
//   override def strictSubTypeOf(sup: Type): Boolean = typ.strictSubTypeOf(sup)
//   override def strictSuperTypeOf(sub: Type) = typ.strictSuperTypeOf(sub)

//  override def text = s"(${typ.text})"
// }

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type) = lower.fold(false)(_.strictSuperTypeOf(sub))

  def fields = upper.fold(BuiltinTypes.objectType.fields)(_.fields)
  def methods = upper.fold(BuiltinTypes.objectType.methods)(_.methods)

  override def text = ???
  override def textRange: TextRange = ???
}

//TODO arrays
case class ArrayType()

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type) = lower.strictSuperTypeOf(sub)

  def fields = upper.fields
  def methods = upper.methods

  override def text = "NOT INFERRED AAA!!!"
  override def textRange: TextRange = ???
}

case class TypeParamList(params: Iterable[TypeParam], val textRange: TextRange)
    extends Tree,
      HasText {
  override def text: String = HasText.seqText(params, ",", "<", ">")
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
  def fields = upperBound.fields
  def methods = upperBound.methods
  override def text = s"$name extends ${upperBound.text} super ${lowerBound.text}"
  override def textRange = TextRange(
      nameRange.start,
      if (!lowerBound.textRange.isSynthetic) lowerBound.textRange.end
      else if (!upperBound.textRange.isSynthetic) upperBound.textRange.end
      else nameRange.end
  )
}

case class TypeParamBound(boundType: BoundType, typeRepr: TypeRef, textRange: TextRange)
    extends HasText {
  //TODO write this
  override def text = ""
}

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}

case class TypeArgList(args: Iterable[Type], val textRange: TextRange) extends Tree, HasText {
  override def text: String = HasText.seqText(args, ",", "<", ">")
}
