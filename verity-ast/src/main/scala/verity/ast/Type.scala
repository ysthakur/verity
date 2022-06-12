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

case class Record(fields: Seq[Field]) extends Type

case class Field(name: String, typ: Type) extends NamedTree, Def
