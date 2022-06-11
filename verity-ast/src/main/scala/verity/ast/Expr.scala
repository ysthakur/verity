package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree, HasType {
  def typ: Type
}

/** A placeholder like ??? */
class Hole extends Expr {
  var typ: Type
}

sealed trait Literal extends Expr

trait BoolLiteral(using file: FileNode) extends Literal {
  def typ: Type = PrimitiveType.BooleanType
}
class TrueLiteral(using file: FileNode) extends BoolLiteral
class FalseLiteral(using file: FileNode) extends BoolLiteral

class NumLiteral(using file: FileNode) extends Literal

case class StringLiteral(text: String) extends Expr {
  def typ: Type = BuiltinTypes.stringTypeDef.makeRef
}

/** A resolved reference to a variable
  */
case class VarRef(varName: Text, decl: VarDef) extends Expr {
  override def typ: Type = decl.typ
}

/** Used for referring to references to packages such as `foo.bar.baz`
  */
case class PkgRef(path: Seq[Text], pkg: Pkg)

case class ParenExpr(expr: Expr) extends Expr {
  def typ: Type = expr.typ
}

case class BinaryExpr(left: Expr, op: Op, right: Expr, typ: Type) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type) extends Expr

/** An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(symbol: String) extends Expr

case class FnCall(
  fn: Expr,
  argLists: List[ArgList],
  typ: Type
) extends Expr

sealed trait ArgList

/** A list of type arguments */
case class TypeArgList(
  args: List[Type]
) extends ArgList

/** A list of normal arguments */
case class NormArgList(
  args: List[Expr]
) extends ArgList

/** A list of non-erased implicit arguments */
case class GivenArgList(
  args: List[Expr]
) extends ArgList

/** A list of erased implicit arguments */
case class ProofArgList(
  args: List[Expr],
) extends ArgList

case class UpcastExpr(expr: Expr, typ: Type) extends Expr
