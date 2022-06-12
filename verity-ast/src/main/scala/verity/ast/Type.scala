package verity.ast

import scala.collection.mutable.ArrayBuffer

trait Type extends Tree

object Type {
  val placeholder: Type = new Type {}
}

//TODO deal with covariance and contravariance?
case class TypeRef(typeDef: TypeDef, args: List[Type] = Nil) extends Type

object UnknownType extends Type

case class Record(fields: Seq[Field]) extends Type

case class Field(name: String, typ: Type) extends Def

case class FunctionType(paramTypes: List[TypeParamList | ValParamList], returnType: Type) extends Type
