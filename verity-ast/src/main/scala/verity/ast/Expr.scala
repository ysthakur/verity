package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree {
  def typ: Type
}

sealed trait Literal extends Expr

trait BoolLiteral(using file: FileNode) extends Literal {
  def typ: Type = TypeRef(BuiltinTypes.boolType)
}
class TrueLiteral(using file: FileNode) extends BoolLiteral
class FalseLiteral(using file: FileNode) extends BoolLiteral

class NumLiteral(value: Double)(using file: FileNode) extends Literal {
  def typ: Type = TypeRef(BuiltinTypes.numType)
}

class CharLiteral(char: Char)(using file: FileNode) extends Literal {
  def typ: Type = TypeRef(BuiltinTypes.charType)
}

case class StringLiteral(text: String)(using file: FileNode) extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.stringType)
}

/** A resolved reference to a variable
  */
case class VarRef(varName: Text, decl: VarDef) extends Expr {
  override def typ: Type = decl.typ
}

/** Used for referring to references to packages such as `foo.bar.baz`
  */
case class PkgRef(path: Seq[Text], pkg: Package)

case class ParenExpr(expr: Expr) extends Expr {
  def typ: Type = expr.typ
}

case class BinaryExpr(left: Expr, op: Op, right: Expr, typ: Type)(using
  file: FileNode
) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type)(using file: FileNode)
    extends Expr

/** An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(symbol: String, var typ: Type)(using file: FileNode) extends Expr

case class FnCall(
  fn: Expr,
  argLists: List[ArgList],
  typ: Type
) extends Expr

sealed trait ArgList

/** A list of type arguments */
case class TypeArgList(args: List[Type]) extends ArgList

/** A list of actual arguments (as opposed to type arguments) */
sealed trait ValArgList extends ArgList {
  def args: List[Expr]
}

/** A list of normal arguments */
case class NormArgList(args: List[Expr]) extends ValArgList

/** A list of non-erased implicit arguments */
case class GivenArgList(args: List[Expr]) extends ValArgList

/** A list of erased implicit arguments */
case class ProofArgList(args: List[Expr]) extends ValArgList

case class UpcastExpr(expr: Expr, typ: Type) extends Expr

/** An expression like `let foo = bar in baz` */
case class LetExpr(vars: List[VarDef], body: Expr, rec: Boolean = false)
    extends Expr {
  def typ = body.typ
}

/** A local variable
  */
class VarDef(
  val name: String,
  var typ: Type,
  var initVal: Expr,
  val isFinal: Boolean = true
) extends Def

case class Lambda(
  paramLists: List[TypeParamList | ValParamList],
  body: Expr,
  returnType: Type
)

case class ValParam(name: String, typ: Type)

case class ValParamList(
  params: List[ValParam],
  kind: ParamListKind = ParamListKind.Normal
)(using file: FileNode)
    extends Tree

enum ParamListKind {
  case Normal, Given, Proof
}
