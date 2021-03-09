package verity.ast.infile

import verity.ast._, infile._
import verity.parsing._

trait Expr extends Node {
  private var _exprType: Option[TypeRepr] = None
  def exprType = _exprType
  def exprType_=(typeRepr: TypeRepr) = _exprType = Some(typeRepr)
  override def toString = text
}

/**
 * Can be on the left hand side of an assignment expression
 */
trait Assignable extends Expr {
  
}

sealed trait Literal extends Expr

enum BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  case TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
  case FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
}
object BoolLiteral {
  def apply(token: Token) =
    if (token.text.charAt(0) == 't') TrueLiteral(token.textRange)
    else FalseLiteral(token.textRange)
}

trait NumLiteral(val numType: NumType) extends Literal

class IntegerLiteral(override val text: String, override val textRange: TextRange, isLong: Boolean) extends NumLiteral(if (isLong) NumType.LONG else NumType.INT)
class FloatLiteral(override val text: String, override val textRange: TextRange, isDouble: Boolean) extends NumLiteral(if (isDouble) NumType.DOUBLE else NumType.FLOAT)

enum NumType {
  case INT
  case LONG
  case FLOAT
  case DOUBLE
}

case class ThisRef(override val textRange: TextRange) extends Expr {
  override def text: String = "this"
}

case class SuperRef(override val textRange: TextRange) extends Expr {
  override def text: String = "super"
}

case class VarRef(varName: Name) extends Expr {
  override def text = varName.text
  override def textRange = varName.textRange
}

case class ParenExpr(expr: Expr, override val textRange: TextRange) extends Expr {
  def text = s"(${expr.text})"
}

case class DotChainedExpr(expr: Expr, propertyName: Name) extends Expr {
  override def text: String = s"${expr.text}.${propertyName.text}"
  override lazy val textRange = expr.textRange to propertyName.textRange
}

case class ArraySelect(arr: Expr, index: Expr, val bracketsTextRange: TextRange) extends Expr {
  override def text: String = s"${arr.text}[${index.text}]"
}

case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr {
  override def text: String = s"(${left.text} $op ${right.text})"
  override def toString = s"( $left $op $right )"
}


case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends Expr {
  override def text: String = s"(${op.text} ${expr.text})"
}

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends Expr {
  override def text: String = s"(${expr.text}${op.text})"
}

case class AssignmentExpr(lhs: Assignable, rhs: Expr, extraOp: Token | Null) extends Expr {
  def text = lhs.text + (if (extraOp == null) "" else extraOp.text) + "=" + rhs.text
  override def textRange = TextRange(lhs.textRange.start, rhs.textRange.end)
}

/**
 * An operator
 * @param symbol
 * @param startOffset
 * @param endOffset
 */
case class Op(symbol: String, override val textRange: TextRange) extends Tree, HasText {
  def isBinary: Boolean = ???
  override def text: String = symbol //symbol.text
}

case class ToBeGiven(typ: Type) extends Expr {
  def text: String = ???
}

case class MethodCall(
  obj: Option[Expr],
  name: Name,
  valArgs: Seq[Expr],
  givenArgs: Seq[Expr],
  typeArgs: Seq[Type],
  override val textRange: TextRange) extends Expr {
  //TODO figure out how to print types
  def text: String = obj.fold("")(_.text + ".") + typeArgs.map(_.toString).mkString("<", ",", ">") + valArgs.map(_.text).mkString("(", ",", ")")
}

case class ArgList(args: List[Argument], override val textRange: TextRange) extends HasText {
  def text = args.view.map(_.text).mkString("(", ",", ")")
}

case class Argument(expr: Expr) extends HasText {
  def text = expr.text
  def textRange = expr.textRange
}