package verity.ast.infile

import verity.ast.{Tree, NamedTree, HasText, TextRange, Text, Synthetic}

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
    override def strictSubTypeOf(sup: Type): Boolean = ???
    override def strictSuperTypeOf(sub: Type): Boolean = ???

    def fields: Iterable[Field] = Nil
    def methods: Iterable[Method] = ??? //collection.mutable.ListBuffer()

    def text: Nothing = ???
    def textRange: Nothing = ???
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
  override def strictSuperTypeOf(sub: Type): Boolean = sub != BuiltinTypes.objectType

  def fields: Iterable[Field] = Nil
  def methods: Iterable[Method] = Nil
  override def text = "Type Nothing"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

case class PrimitiveTypeRef(typ: PrimitiveType, textRange: TextRange) extends Type {
  def fields: Iterable[Field] = Nil
  def methods: Iterable[Method] = Nil

  def text: String = typ.text
}

enum PrimitiveType extends Type, Synthetic {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???

  def fields: Iterable[Field] = Nil
  def methods: Iterable[Method] = Nil

  override def text: String = this.toString.toLowerCase.nn
}
object PrimitiveType {
  lazy val numericTypes: TypeUnion = TypeUnion(List(BYTE, CHAR, SHORT, INT, FLOAT, LONG, DOUBLE))

  val fromName: String => Option[PrimitiveType] =
    PrimitiveType.values.view.map(typ => typ.text -> typ).toMap.get
}

case class TypeRef(
    path: Seq[Text],
    args: TypeArgList,
    private[this] var _resolved: Option[TypeDef] = None
) extends Type {
  def resolved: Option[TypeDef] = _resolved
  private[verity] def resolved_=(typeDef: TypeDef): Unit = _resolved = Some(typeDef)

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???

  def fields: Iterable[Field] = resolved.fold(Nil)(_.fields)
  def methods: Iterable[Method] = resolved.fold(Nil)(_.methods)

  override def text: String = HasText.seqText(path, ".") + args.text

  override def textRange: TextRange =
    if (args.isEmpty || args.textRange.isSynthetic) TextRange(path.head.textRange.start, path.last.textRange.end)
    else TextRange(path.head.textRange.start, args.textRange.end)

  override def equals(other: Any): Boolean = other match {
    case tr: TypeRef =>
      resolved.flatMap(typ => tr.resolved.map(_ == typ)).getOrElse(false) && this.args.args.size == tr.args.args.size && this.args.args
        .lazyZip(tr.args.args)
        .forall(_ == _)
    case _ => false
  }
}

case class TypeUnion(types: Iterable[Type]) extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???

  def fields: Iterable[Field] = ???
  def methods: Iterable[Method] = ???

  def text: Nothing = ???
}

//todo is this necessary?
case class TypeRange(var upper: Type, var lower: Type) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type): Boolean = lower.strictSuperTypeOf(sub)

  def fields: Iterable[Field] = upper.fields
  def methods: Iterable[Method] = upper.methods

  override def text: Nothing = ???
  override def textRange: TextRange = ???
}

// case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
//   override def strictSubTypeOf(sup: Type): Boolean = typ.strictSubTypeOf(sup)
//   override def strictSuperTypeOf(sub: Type) = typ.strictSuperTypeOf(sub)

//  override def text = s"(${typ.text})"
// }

case class Wildcard(upper: Option[TypeRef], lower: Option[TypeRef]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = lower.fold(false)(_.strictSuperTypeOf(sub))

  def fields: Iterable[Field] = upper.fold(BuiltinTypes.objectType.fields)(_.fields)
  def methods: Iterable[Method] = upper.fold(BuiltinTypes.objectType.methods)(_.methods)

  override def text: Nothing = ???
  override def textRange: TextRange = ???
}

//TODO arrays
case class ArrayType()

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type): Boolean = lower.strictSuperTypeOf(sub)

  def fields: Iterable[Field] = upper.fields
  def methods: Iterable[Method] = upper.methods

  override def text = "NOT INFERRED AAA!!!"
  override def textRange: TextRange = ???
}

object UnknownType extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  def fields: Iterable[Field] = Nil
  def methods: Iterable[Method] = Nil

  override def text = "<Unknown Type>"
  override def textRange: TextRange = TextRange.synthetic
}

case class TypeParamList(params: Iterable[TypeParam], textRange: TextRange) extends Tree, HasText {
  override def text: String = HasText.seqText(params, ",", "<", ">")
}

object TypeParamList {
  def empty: TypeParamList = TypeParamList(Seq.empty, TextRange.synthetic)
}

class TypeParam(
    val name: String,
    val upperBound: Type,
    val lowerBound: Type,
    val nameRange: TextRange
) extends HasText,
      NamedTree,
      TypeDef {
  def fields: Iterable[Field] = upperBound.fields
  def methods: Iterable[Method] = upperBound.methods
  override def text = s"$name extends ${upperBound.text} super ${lowerBound.text}"
  override def textRange: TextRange = TextRange(
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

case class TypeArgList(args: Iterable[Type], textRange: TextRange) extends Tree, HasText {
  def isEmpty: Boolean = args.isEmpty
  override def text: String = HasText.seqText(args, ",", "<", ">")
}

given Empty[TypeArgList] with
  def empty: TypeArgList = TypeArgList(Nil, TextRange.synthetic)
