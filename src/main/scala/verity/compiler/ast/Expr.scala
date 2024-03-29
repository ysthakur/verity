package verity.compiler.ast

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree

case class BoolLiteral(value: Boolean, span: Span = Span.synthetic) extends Expr

case class IntLiteral(value: Int, span: Span = Span.synthetic) extends Expr

case class DoubleLiteral(value: Double, span: Span = Span.synthetic)
    extends Expr

case class CharLiteral(char: Char, span: Span = Span.synthetic) extends Expr

case class StringLiteral(text: String, span: Span = Span.synthetic) extends Expr

/** A resolved reference to a variable */
case class VarRef(varName: String, decl: VarDef | Null, span: Span = Span.synthetic)
    extends Expr

case class UnresolvedIdentifier(id: String, span: Span = Span.synthetic)
    extends Expr

/** Used for accessing a property on an object like `foo.bar`.
  *
  * @param obj
  *   The object whose field is being accessed
  * @param field
  *   The field name
  */
case class FieldAccess(obj: Expr, field: String) extends Expr

case class ParenExpr(expr: Expr, span: Span) extends Expr

case class BinExpr(left: Expr, op: Op, right: Expr) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type) extends Expr

/** An operator */
case class Op(symbol: String, span: Span = Span.synthetic) extends Expr

/** An if-else expression */
case class If(cond: Expr, thenBody: Expr, elseBody: Expr) extends Expr

case class FnCall(fn: Expr, comptimeArgs: ComptimeArgs, args: Args) extends Expr

/** A list of actual arguments (as opposed to type arguments) */
case class ValArgList(args: List[Expr], isGiven: Boolean)

/** An expression like `let foo = bar in baz` */
case class LetExpr(vars: List[VarDef], body: Expr) extends Expr

case class Lambda(
    comptimeParams: ComptimeParams,
    params: Params,
    body: Expr,
    span: Span
) extends Expr

/** The usual kind of runtime parameters, both normal and implicit ones */
case class Params(
    normParams: List[Param],
    givenParams: List[Param]
)

object Params {
  def empty = Params(Nil, Nil)
}

/** Runtime arguments, both normal and implicit ones */
case class Args(normArgs: List[Expr], givenArgs: List[Expr])

object Args {
  def empty = Args(Nil, Nil)
}

case class ValParamList(params: List[Param], isGiven: Boolean) extends Tree

case class Modifier(mod: String, span: Span) extends Tree

case class AssignExpr(lvalue: Expr, rvalue: Expr) extends Expr
