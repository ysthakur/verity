package verity.compiler.ast

import scala.collection.mutable.ArrayBuffer

sealed trait Expr extends Tree

case class BoolLiteral(value: Boolean, textRange: TextRange) extends Expr

case class IntLiteral(value: Int, textRange: TextRange) extends Expr

case class DoubleLiteral(value: Double, textRange: TextRange) extends Expr

case class CharLiteral(char: Char, textRange: TextRange) extends Expr

case class StringLiteral(text: String, textRange: TextRange) extends Expr

/** A resolved reference to a variable
  */
case class VarRef(varName: String, decl: VarDef, textRange: TextRange)
    extends Expr

case class UnresolvedIdentifier(id: String, textRange: TextRange) extends Expr

/** Used for referring to references to packages such as `foo.bar.baz`
  */
case class PkgRef(path: Seq[Text])

/** Used for accessing a property on an object like `foo.bar`.
  *
  * @param obj
  *   The object whose field is being accessed
  * @param field
  *   The field name
  */
case class FieldAccess(obj: Expr, field: String) extends Expr

case class ParenExpr(expr: Expr, textRange: TextRange) extends Expr

case class BinExpr(left: Expr, op: Op, right: Expr) extends Expr

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type) extends Expr

/** An operator */
case class Op(symbol: String, textRange: TextRange) extends Expr

/** An if-else expression */
case class If(cond: Expr, thenBody: Expr, elseBody: Expr) extends Expr

case class FnCall(
  fn: Expr,
  constArgss: List[ConstArgList],
  valArgss: List[ValArgList]
) extends Expr

/** A list of actual arguments (as opposed to type arguments) */
case class ValArgList(args: List[Expr], isGiven: Boolean)

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
  constParamss: List[ConstParamList],
  valParamss: List[ValParamList],
  body: Expr,
  textRange: TextRange
) extends Expr

case class ValParam(name: String, typ: Type)

case class ValParamList(
  params: List[ValParam],
  isGiven: Boolean
) extends Tree

case class Modifier(mod: String, textRange: TextRange) extends Tree

case class AssignExpr(lvalue: Expr, rvalue: Expr) extends Expr
