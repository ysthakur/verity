package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree

class BoolLiteral(value: Boolean, textRange: TextRange)(using file: FileNode)
    extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.boolType)
}

class IntLiteral(value: Int, textRange: TextRange)(using file: FileNode)
    extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.intType)
}

class DoubleLiteral(value: Double, textRange: TextRange)(using file: FileNode)
    extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.doubleType)
}

class CharLiteral(char: Char, textRange: TextRange)(using file: FileNode)
    extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.charType)
}

case class StringLiteral(text: String, textRange: TextRange)(using
  file: FileNode
) extends Expr {
  def typ: Type = TypeRef(BuiltinTypes.stringType)
}

case class NullLiteral(textRange: TextRange)(using file: FileNode) extends Expr

case class ThisRef(textRange: TextRange)(using file: FileNode) extends Expr

case class SuperRef(textRange: TextRange)(using file: FileNode) extends Expr

/** A resolved reference to a variable
  */
case class VarRef(varName: String, decl: VarDef, textRange: TextRange)
    extends Expr

case class UnresolvedIdentifier(id: String, textRange: TextRange) extends Expr

/** Used for referring to references to packages such as `foo.bar.baz`
  */
case class PkgRef(path: Seq[Text], pkg: Package)

/** Used for accessing a property on an object like `foo.bar`.
  *
  * @param obj
  *   The object whose property is being accessed
  * @param prop
  *   The property name
  */
case class PropAccess(obj: Expr, prop: String) extends Expr

case class ParenExpr(expr: Expr, textRange: TextRange) extends Expr

case class BinExpr(left: Expr, op: Op, right: Expr)(using
  file: FileNode
) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type)(using file: FileNode)
    extends Expr

/** An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(symbol: String)(using file: FileNode) extends Expr

case class FnCall(
  fn: Expr,
  typeArgs: Option[TypeArgList],
  normArgs: NormArgList,
  givenArgs: Option[GivenArgList],
  proofArgs: Option[ProofArgList]
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
    extends Expr

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

case class Modifier(mod: String, textRange: TextRange) extends Tree
