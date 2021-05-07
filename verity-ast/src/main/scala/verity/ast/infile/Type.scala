package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

trait Type extends Tree, HasText {

  def fields: Iterable[Field]
  def methods: Iterable[Method]
  def superTypes: Iterable[Type]

  def allMethods: (Iterable[Method], Iterable[Iterable[Method]]) =
    Method.mergeMethods(superTypes.view.flatMap(_.allMethods._1) ++ this.methods)

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

    override def fields: Iterable[Field] = Nil
    override def methods: Iterable[Method] = ??? //collection.mutable.ListBuffer()

    override def superTypes: Iterable[Type] = ???

    override def text: Nothing = ???
    override def textRange: Nothing = ???
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

  //todo figure out how to deal with this
  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def text = "Type Nothing"
}

class VoidTypeRef(val textRange: TextRange) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def text = "void"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

case class PrimitiveType(typ: PrimitiveTypeDef, textRange: TextRange) extends Type {
  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil

  override def superTypes: Iterable[Type] = Nil

  override def superTypeOf(other: Type): Boolean = this == other
  override def subTypeOf(other: Type): Boolean = this == other
  override def strictSuperTypeOf(other: Type): Boolean = false
  override def strictSubTypeOf(other: Type): Boolean = false

  override def equals(other: Any): Boolean = other match {
    case PrimitiveType(t, _) => this.typ == t
    case _                   => false
  }
  override def text: String = typ.text
}
object PrimitiveType {
  val BooleanType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.BOOLEAN, TextRange.synthetic)
  val ByteType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.BYTE, TextRange.synthetic)
  val CharType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.CHAR, TextRange.synthetic)
  val ShortType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.SHORT, TextRange.synthetic)
  val IntType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.INT, TextRange.synthetic)
  val FloatType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.FLOAT, TextRange.synthetic)
  val LongType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.LONG, TextRange.synthetic)
  val DoubleType: PrimitiveType = PrimitiveType(PrimitiveTypeDef.DOUBLE, TextRange.synthetic)
}

enum PrimitiveTypeDef extends Type, Synthetic {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???
  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil

  override def text: String = this.toString.toLowerCase.nn
}
object PrimitiveTypeDef {
  lazy val numericTypes: TypeUnion = TypeUnion(List(BYTE, CHAR, SHORT, INT, FLOAT, LONG, DOUBLE))

  val fromName: String => Option[PrimitiveTypeDef] =
    PrimitiveTypeDef.values.view.map(typ => typ.text -> typ).toMap.get
}

case class ResolvedTypeRef(
    path: Seq[Text],
    args: TypeArgList,
    resolved: TypeDef
) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???

  override def fields: Iterable[Field] = resolved.fields
  override def methods: Iterable[Method] = resolved.methods
  override def superTypes: Iterable[Type] = resolved.superTypes

  override def text: String = HasText.seqText(path, ".") + args.text

  override def textRange: TextRange =
    TextRange(
        path.head.textRange.start,
        (if args.isEmpty || args.isSynthetic then path.last else args).textRange.end
    )

  override def equals(other: Any): Boolean = other match {
    case tr: ResolvedTypeRef =>
      resolved == tr.resolved &&
        this.args.args.size == tr.args.args.size &&
        this.args.args.lazyZip(tr.args.args).forall(_ == _)
    case _ => false
  }
}

case class TypeUnion(types: Iterable[Type]) extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type): Boolean = ???
  override def strictSuperTypeOf(sub: Type): Boolean = ???

  override def superTypes: Set[Type] = types.map(_.superTypes.toSet).reduceLeft(_ union _)

  override def fields: Iterable[Field] = ???
  override def methods: Iterable[Method] = ???

  override def text: Nothing = ???
}

//todo is this necessary?
//case class TypeRange(var upper: Type, var lower: Type) extends Type {
//  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
//  override def strictSuperTypeOf(sub: Type): Boolean = lower.strictSuperTypeOf(sub)
//
//  override def fields: Iterable[Field] = upper.fields
//  override def methods: Iterable[Method] = upper.methods
//
//  override def text: Nothing = ???
//  override def textRange: TextRange = ???
//}

// case class ParenType(typ: Type, override val textRange: TextRange) extends Type {
//   override def strictSubTypeOf(sup: Type): Boolean = typ.strictSubTypeOf(sup)
//   override def strictSuperTypeOf(sub: Type) = typ.strictSuperTypeOf(sub)

//  override def text = s"(${typ.text})"
// }

case class Wildcard(upper: Option[ResolvedTypeRef], lower: Option[ResolvedTypeRef]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = lower.fold(false)(_.strictSuperTypeOf(sub))

  override def fields: Iterable[Field] = upper.fold(BuiltinTypes.objectType.fields)(_.fields)
  override def methods: Iterable[Method] = upper.fold(BuiltinTypes.objectType.methods)(_.methods)

  override def superTypes: Iterable[Type] = upper.fold(Nil)(_.superTypes)

  override def text: Nothing = ???
  override def textRange: TextRange = ???
}

//TODO arrays
case class ArrayType(elemType: Type, private[this] val bracketRange: TextRange) extends Type {
  val fields: Iterable[Field] = List(Field(Text("length"), ListBuffer.empty, PrimitiveType.IntType))
  override def methods: Iterable[Method] = List()

  override def superTypes: Iterable[Type] = BuiltinTypes.objectType :: Nil

  override def strictSubTypeOf(sup: Type): Boolean = sup == BuiltinTypes.objectType
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def text = s"${elemType.text}[]"
  override def textRange: TextRange = TextRange(elemType.textRange.start, bracketRange.end)
}

object UnknownType extends Type {
  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def superTypes: Iterable[Type] = Nil

  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def text = "<Unknown Type>"
  override def textRange: TextRange = TextRange.synthetic
}

case class TypeParamList(params: Iterable[TypeParam], textRange: TextRange) extends Tree, HasText {
  override def text: String = HasText.seqText(params, ",", "<", ">")
}

given Empty[TypeParamList] with
  val empty: TypeParamList = TypeParamList(Seq.empty, TextRange.synthetic)

class TypeParam(
    val name: String,
    val upperBound: Type,
    val lowerBound: Type,
    val nameRange: TextRange
) extends HasText,
      NamedTree,
      TypeDef {
  override def fields: Iterable[Field] = upperBound.fields
  override def methods: Iterable[Method] = upperBound.methods

  override def superTypes: Iterable[Type] = upperBound.superTypes.toSeq :+ upperBound

  /** No higher-kinded types, at least for now
    */
  def typeParams: TypeParamList = Empty[TypeParamList]

  override def text = s"$name extends ${upperBound.text} super ${lowerBound.text}"
  override def textRange: TextRange = TextRange(
      nameRange.start,
      if !lowerBound.textRange.isSynthetic then lowerBound.textRange.end
      else if !upperBound.textRange.isSynthetic then upperBound.textRange.end
      else nameRange.end
  )
}

case class TypeParamBound(boundType: BoundType, typeRepr: ResolvedTypeRef, textRange: TextRange)
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
