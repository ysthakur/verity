package verity.compiler.ast

import verity.compiler.ast.*

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree

case class BoolLiteral(value: Boolean, textRange: TextRange) extends Expr

case class IntLiteral(value: Int, textRange: TextRange) extends Expr

case class DoubleLiteral(value: Double, textRange: TextRange) extends Expr

case class CharLiteral(char: Char, textRange: TextRange) extends Expr

case class StringLiteral(text: String, textRange: TextRange) extends Expr

case class NullLiteral(textRange: TextRange) extends Expr

case class ThisRef(textRange: TextRange) extends Expr

case class SuperRef(textRange: TextRange) extends Expr

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

case class BinExpr(left: Expr, op: Op, right: Expr) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type) extends Expr

/** An operator */
case class Op(symbol: String, textRange: TextRange) extends Expr

case class FnCall(
  fn: Expr,
  typeArgs: Option[TypeArgList],
  normArgs: NormArgList,
  givenArgs: Option[GivenArgList],
  proofArgs: Option[ProofArgList]
) extends Expr

sealed trait ArgList

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
case class LetExpr(vars: List[VarDef], body: Expr) extends Expr

/** A local variable
  */
case class VarDef(
  val name: String,
  val typ: Option[Type],
  val value: Expr
) extends Def

case class Lambda(
  typeParamList: Option[TypeParamList],
  normParamList: Option[ValParamList],
  givenParamList: Option[ValParamList],
  proofParamList: Option[ValParamList],
  body: Expr,
  textRange: TextRange
)

case class ValParam(name: String, typ: Type)

case class ValParamList(
  params: List[ValParam],
  kind: ParamListKind = ParamListKind.Normal
) extends Tree

enum ParamListKind {
  case Normal, Given, Proof
}

case class Modifier(mod: String, textRange: TextRange) extends Tree

case class AssignExpr(lvalue: Expr, rvalue: Expr) extends Expr
