package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer
import scala.annotation.constructorOnly


case class Lambda(
  val paramLists: List[ValParamList],
  val body: Expr,
  val returnType: Type
)

case class ValParam(name: String, typ: Type)

case class ValParamList(
  params: List[ValParam],
  kind: ParamListKind = ParamListKind.Normal
)(using @constructorOnly file: FileNode) extends Tree

enum ParamListKind {
  case Normal, Given, Proof
}
