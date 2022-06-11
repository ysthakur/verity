package verity.ast

import scala.collection.mutable.ArrayBuffer

trait Type extends Tree

object Type {
  val placeholder: Type = new Type {}

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
      case _ => typ
    }

    helper(typ)
  }
}

class VoidTypeRef(using file: FileNode) extends Type

enum PrimitiveType(val typ: PrimitiveTypeDef) extends Type {
  case BooleanType extends PrimitiveType(PrimitiveTypeDef.Boolean)
  case ByteType extends PrimitiveType(PrimitiveTypeDef.Byte)
  case CharType extends PrimitiveType(PrimitiveTypeDef.Char)
  case ShortType extends PrimitiveType(PrimitiveTypeDef.Short)
  case IntType extends PrimitiveType(PrimitiveTypeDef.Int)
  case FloatType extends PrimitiveType(PrimitiveTypeDef.Float)
  case LongType extends PrimitiveType(PrimitiveTypeDef.Long)
  case DoubleType extends PrimitiveType(PrimitiveTypeDef.Double)
}

object PrimitiveType {
  lazy val numericTypes: Set[Type] =
    Set(ByteType, CharType, ShortType, IntType, FloatType, LongType, DoubleType)
}

enum PrimitiveTypeDef extends TypeDef, Synthetic {
  case Boolean, Byte, Short, Char, Int, Float, Long, Double

  override def typeParams = TypeParamList(Nil)

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
) extends Type {
  override def equals(other: Any): Boolean = other match {
    case tr: ResolvedTypeRef =>
      typeDef == tr.typeDef && this.args == tr.args
    case _ => false
  }
}

object UnknownType extends Type

case class TypeParamList(params: Iterable[TypeParam]) extends Tree

given Empty[TypeParamList] with
  val empty: TypeParamList = TypeParamList(Seq.empty)

class TypeParam(
  val name: String
) extends NamedTree, TypeDef {

  /** No higher-kinded types, at least for now
    */
  def typeParams: TypeParamList = Empty[TypeParamList]
}
