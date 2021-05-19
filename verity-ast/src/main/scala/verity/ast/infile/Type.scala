package verity.ast.infile

import verity.ast._

import scala.collection.mutable.ArrayBuffer

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
    override def methods: Iterable[Method] = ??? //collection.mutable.ArrayBuffer()

    override def superTypes: Iterable[Type] = ???

    override def text: Nothing = ???
  }

  def equal(t1: Type, t2: Type): Boolean = ???
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

class VoidTypeRef(override val textRange: TextRange) extends Type, HasTextRange {
  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def text = "void"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

case class PrimitiveType(typ: PrimitiveTypeDef, override val textRange: TextRange)
    extends Type,
      HasTextRange {
  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil

  override def superTypes: Iterable[Type] = Nil

  override def superTypeOf(other: Type): Boolean = this == other
  override def subTypeOf(other: Type): Boolean = this == other
  override def strictSuperTypeOf(other: Type): Boolean = false
  override def strictSubTypeOf(other: Type): Boolean = false

  override def equals(other: Any): Boolean =
    other match {
      case PrimitiveType(t, _) =>
        println(s"${this.typ == t}, this=$this, ${this.getClass}, other=$other, ${other.getClass}")
        this.typ == t
      case _ => false
    }
  override def text: String = typ.text
}
object PrimitiveType {
  val BooleanType = PrimitiveType(PrimitiveTypeDef.BOOLEAN, TextRange.synthetic)
  val ByteType = PrimitiveType(PrimitiveTypeDef.BYTE, TextRange.synthetic)
  val CharType = PrimitiveType(PrimitiveTypeDef.CHAR, TextRange.synthetic)
  val ShortType = PrimitiveType(PrimitiveTypeDef.SHORT, TextRange.synthetic)
  val IntType = PrimitiveType(PrimitiveTypeDef.INT, TextRange.synthetic)
  val FloatType = PrimitiveType(PrimitiveTypeDef.FLOAT, TextRange.synthetic)
  val LongType = PrimitiveType(PrimitiveTypeDef.LONG, TextRange.synthetic)
  val DoubleType = PrimitiveType(PrimitiveTypeDef.DOUBLE, TextRange.synthetic)
}

enum PrimitiveTypeDef extends TypeDef, Synthetic {
  case BOOLEAN, BYTE, SHORT, CHAR, INT, FLOAT, LONG, DOUBLE

//  override def subTypeDefOf(sup: Type): Boolean = this == sup
//  override def superTypeOf(sub: Type): Boolean = this == sub
//  override def strictSubTypeOf(sup: Type): Boolean = false
//  override def strictSuperTypeOf(sub: Type): Boolean = false
  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil

  override def methods: Iterable[Method] = Nil

  override def typeParams: TypeParamList = TypeParamList(Nil, TextRange.synthetic)

  override def text: String = this.name

  override def name: String = this.toString.toLowerCase.nn
}
object PrimitiveTypeDef {
  lazy val numericTypes: Set[PrimitiveTypeDef] = Set(BYTE, CHAR, SHORT, INT, FLOAT, LONG, DOUBLE)
  lazy val numericType: TypeUnion = TypeUnion(
    numericTypes.map(pt => PrimitiveType(pt, TextRange.synthetic))
  )

  val fromName: String => Option[PrimitiveTypeDef] =
    PrimitiveTypeDef.values.view.map(typ => typ.name -> typ).toMap.get
}

trait ResolvedOrUnresolvedTypeRef extends Type

//TODO deal with covariance and contravariance?
case class ResolvedTypeRef(
  path: Seq[Text],
  args: TypeArgList,
  typeDef: TypeDef
) extends Type,
      ResolvedOrUnresolvedTypeRef {
  //todo deal with wildcards
  override def strictSubTypeOf(sup: Type): Boolean = sup match {
    case ResolvedTypeRef(_, _, typeDef2) => typeDef.strictSubTypeDefOf(typeDef2)
    case _                               => false
  }
  override def strictSuperTypeOf(sub: Type): Boolean = sub match {
    case ResolvedTypeRef(_, _, typeDef2) => typeDef2.strictSubTypeDefOf(typeDef)
    case _                               => false
  }

  override def fields: Iterable[Field] = typeDef.fields
  override def methods: Iterable[Method] = typeDef.methods
  override def superTypes: Iterable[Type] = typeDef.superTypes

  override def text: String = HasText.seqText(path, ".") + args.text

  /*override def textRange: TextRange =
    TextRange(
      path.head.textRange.start,
      (if (args.isEmpty || args.isSynthetic) path.last else args).textRange.end
    )*/

  override def equals(other: Any): Boolean = other match {
    case tr: ResolvedTypeRef =>
      typeDef == tr.typeDef && this.args == tr.args
    case _ => false
  }
}

case class TypeUnion(types: Set[Type]) extends Type, Synthetic {
  override def strictSubTypeOf(sup: Type): Boolean = types.forall(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = types.forall(_.strictSuperTypeOf(sub))

  override def superTypes: Set[Type] = types.view.map(_.superTypes.toSet).reduceLeft(_ union _)

  override def fields: Iterable[Field] = types.view.map(_.fields.toSet).reduceLeft(_.toSet union _)
  override def methods: Iterable[Method] = Method.mergeMethods(types.flatMap(_.methods))._1

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
//  // override def textRange = ???
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
  // override def textRange = ???
}

//TODO arrays
case class ArrayType(elemType: Type, private[this] val bracketRange: TextRange) extends Type {
  val fields: Iterable[Field] = List(
    Field(Text("length"), ArrayBuffer.empty, PrimitiveType.IntType)
  )
  override def methods: Iterable[Method] = List()

  override def superTypes: Iterable[Type] = BuiltinTypes.objectType :: Nil

  override def strictSubTypeOf(sup: Type): Boolean = sup == BuiltinTypes.objectType
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def text = s"${elemType.text}[]"
//  override def textRange: TextRange = TextRange(elemType.textRange.start, bracketRange.end)
}

object UnknownType extends Type {
  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def superTypes: Iterable[Type] = Nil

  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def text = "<Unknown Type>"
}

case class TypeParamList(params: Iterable[TypeParam], override val textRange: TextRange)
    extends Tree,
      HasTextRange {
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

  override def superTypes: Iterable[Type] =
    upperBound.superTypes.toSeq :+ upperBound

  /** No higher-kinded types, at least for now
    */
  def typeParams: TypeParamList = Empty[TypeParamList]

  override def text = s"$name extends ${upperBound.text} super ${lowerBound.text}"
  /*override def textRange: TextRange = TextRange(
    nameRange.start,
    if (!lowerBound.textRange.isSynthetic) lowerBound.textRange.end
    else if (!upperBound.textRange.isSynthetic) upperBound.textRange.end
    else nameRange.end
  )*/
}

case class TypeParamBound(
  boundType: BoundType,
  typeRepr: ResolvedTypeRef,
  val boundRange: TextRange
) extends HasText {
  //TODO write this
  override def text = ""
}

enum BoundType {
  case EXTENDS, SUPER, NOT_EXTENDS, NOT_SUPER
}

case class TypeArgList(args: Iterable[Type], override val textRange: TextRange)
    extends Tree,
      HasTextRange {
  def isEmpty: Boolean = args.isEmpty
  override def text: String = if (args.isEmpty) "" else HasText.seqText(args, ",", "<", ">")
  override def equals(other: Any): Boolean = other match {
    case TypeArgList(otherArgs, _) =>
      args.size == otherArgs.size && args.lazyZip(otherArgs).forall(_ == _)
    case _ => false
  }
}

given Empty[TypeArgList] with
  def empty: TypeArgList = TypeArgList(Nil, TextRange.synthetic)
