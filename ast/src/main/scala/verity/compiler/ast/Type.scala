package verity.compiler.ast

import scala.collection.mutable.ArrayBuffer

trait Type extends Tree

object Type {
  val placeholder: Type = new Type {}
}

/** An unresolved type in the form `foo.bar.Baz`. Has to be like this in case
  * the stuff at the start is a package instead of a type.
  *
  * @param path
  */
case class UnresolvedType(path: List[String]) extends Type

//TODO deal with covariance and contravariance?
case class TypeRef(typeDef: TypeDef, args: List[Type] = Nil) extends Type

case class ParenType(inner: Type, textRange: TextRange) extends Type

/** A list of type arguments */
case class TypeArgList(args: List[Type]) extends Tree

case class TypeApply(typeCtor: Type, args: TypeArgList) extends Type

/** Accessing a type member of another type (e.g. `Foo.Bar`)
  *
  * @param typ
  * @param memberName
  *   The name of the member
  */
case class TypeMemberAccess(typ: Type, memberName: String) extends Type

case object UnknownType extends Type

case class FunctionType(
  comptimeParamss: List[ComptimeParamList],
  normParamTypes: List[Type],
  givenParamTypes: List[Type],
  returnType: Type
) extends Type

/** Compile-time arguments */
case class ComptimeArgs(
  typeArgs: List[Type],
  normConstArgs: List[Expr],
  givenConstArgs: List[Expr]
)

object ComptimeArgs {
  def empty = ComptimeArgs(Nil, Nil, Nil)
}

object Comptime {
  def empty = ComptimeArgs(Nil, Nil, Nil)
}
