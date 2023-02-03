package verity.compiler.ast

import scala.collection.mutable.ArrayBuffer

import cats.data.NonEmptyList

trait Type extends Tree

object Type {
  val placeholder: Type = new Type {}
}

/** An unresolved type in the form `foo.bar.Baz`. Has to be like this in case
  * the stuff at the start is a package instead of a type.
  */
case class UnresolvedType(path: NonEmptyList[String]) extends Type

object UnresolvedType {
  /** Helper to make unresolved types more concisely */
  def apply(pathStart: String, pathRest: String*): UnresolvedType =
    UnresolvedType(NonEmptyList(pathStart, pathRest.toList))
}

/** A type that needs to be inferred */
object ToBeInferred extends Type {
  override def toString = "ToBeInferred"
}

//TODO deal with covariance and contravariance?
case class TypeRef(typeDef: TypeDef, args: List[Type] = Nil) extends Type

case class ParenType(inner: Type, span: Span) extends Type

case class TypeApply(typeCtor: Type, args: List[Type]) extends Type

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
