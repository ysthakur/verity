package verity.ast

import scala.collection.mutable.ArrayBuffer

trait Type extends Tree, HasText {
  override def toString = text
}

object Type {
  val placeholder: Type = new Type {
    override def text = ???
  }

  def equal(t1: Type, t2: Type): Boolean = ???

  /** Fill in type arguments
    * @param params
    *   The type parameters to a class or method
    * @param typeArgs
    *   The values of those type parameters
    * @param typ
    *   A type that uses references to type parameters `params`
    */
  def fillArgs(params: Iterable[TypeParam], typeArgs: Iterable[Type])(typ: Type): Type = {
    // Keys are parameters, values are arguments
    val argMap = params.lazyZip(typeArgs).toMap
    def helper(typ: Type): Type = typ match {
      case typeRef: ResolvedTypeRef =>
        typeRef.copy(args = typeRef.args.copy(args = typeRef.args.args.map(helper)))
      case arrayType: ArrayType =>
        arrayType.copy(elemType = helper(arrayType.elemType))
      case _ => typ
    }

    helper(typ)
  }
}

// given ToJava[AnyType.type] = _ => "Type Any"

// given ToJava[ObjectType.type] = _ => "Type Object"

class VoidTypeRef(override val textRange: TextRange) extends Type, HasTextRange {
  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

  override def superTypes: Iterable[Type] = Nil

  override def fields: Iterable[Field] = Nil
  override def methods: Iterable[Method] = Nil
  override def text = "void"
}

// given ToJava[NothingType.type] = _ => "Type Nothing"

enum PrimitiveType(val typ: PrimitiveTypeDef) extends Type, HasTextRange {
  case BooleanType extends PrimitiveType(PrimitiveTypeDef.Boolean)
  case ByteType extends PrimitiveType(PrimitiveTypeDef.Byte)
  case CharType extends PrimitiveType(PrimitiveTypeDef.Char)
  case ShortType extends PrimitiveType(PrimitiveTypeDef.Short)
  case IntType extends PrimitiveType(PrimitiveTypeDef.Int)
  case FloatType extends PrimitiveType(PrimitiveTypeDef.Float)
  case LongType extends PrimitiveType(PrimitiveTypeDef.Long)
  case DoubleType extends PrimitiveType(PrimitiveTypeDef.Double)

  override val textRange = TextRange.synthetic

  override def text: String = typ.text
}

object PrimitiveType {
  lazy val numericTypes: Set[Type] =
    Set(ByteType, CharType, ShortType, IntType, FloatType, LongType, DoubleType)
  lazy val numericType: TypeUnion = TypeUnion(numericTypes)
}

enum PrimitiveTypeDef extends TypeDef, Synthetic {
  case Boolean, Byte, Short, Char, Int, Float, Long, Double

  override def text: String = this.name

  override def name: String = this.toString.toLowerCase.nn
}
object PrimitiveTypeDef {
  val fromName: String => Option[PrimitiveTypeDef] =
    PrimitiveTypeDef.values.view.map(typ => typ.name -> typ).toMap.get
}

//TODO deal with covariance and contravariance?
case class ResolvedTypeRef(
  path: Seq[Text],
  args: TypeArgList,
  typeDef: TypeDef
) extends Type,
      TypeRef {
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

object UnknownType extends Type {
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
