package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

sealed trait Expr extends Tree, HasText {
  private var _exprType: Type = ToBeInferred(AnyType, NothingType, List.empty)
  def exprType = _exprType
  private[verity] def exprType_=(typ: Type) = _exprType = typ
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

sealed trait NumLiteral(val numType: NumType) extends Literal

case class IntegerLiteral(
  override val text: String,
  override val textRange: TextRange,
  isLong: Boolean
) extends NumLiteral(if (isLong) NumType.LONG else NumType.INT)

case class FloatLiteral(
  override val text: String, 
  override val textRange: TextRange, 
  isDouble: Boolean
) extends NumLiteral(if (isDouble) NumType.DOUBLE else NumType.FLOAT)

enum NumType {
  case INT
  case LONG
  case FLOAT
  case DOUBLE
}

class StringLiteral(override val text: String, override val textRange: TextRange) extends Expr

class ThisRef(override val textRange: TextRange) extends Expr {
  override def text: String = "this"
}

class SuperRef(override val textRange: TextRange) extends Expr {
  override def text: String = "super"
}

class VarRef(val varName: String, val textRange: TextRange) extends Expr {
  override def text = varName
}

class ParenExpr(expr: Expr, override val textRange: TextRange) extends Expr {
  def text = s"(${expr.text})"
}

class DotChainedExpr(expr: Expr, propertyName: String, propertyNameRange: TextRange) extends Expr {
  override def text: String = s"${expr.text}.${propertyName}"
  override lazy val textRange = TextRange(expr.textRange.start, propertyNameRange.end)
}

case class ArraySelect(arr: Expr, index: Expr, val bracketsTextRange: TextRange) extends Expr {
  override def text: String = s"${arr.text}[${index.text}]"
  override def textRange = TextRange(arr.textRange.start, bracketsTextRange.end)
}

case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr {
  override def text: String = s"(${left.text} $op ${right.text})"
  override def textRange = TextRange(left.textRange.start, right.textRange.end)
}


case class UnaryPreExpr(op: Op, expr: Expr) extends Expr {
  override def text: String = s"(${op.text} ${expr.text})"
  override def textRange = TextRange(op.textRange.start, expr.textRange.end)
}

case class UnaryPostExpr(expr: Expr, op: Op) extends Expr {
  override def text: String = s"(${expr.text}${op.text})"
  override def textRange = TextRange(expr.textRange.start, op.textRange.end)
}

case class AssignmentExpr(lhs: Expr, rhs: Expr, extraOp: Option[Token]) extends Expr {
  def text = lhs.text + extraOp.fold("")(_.text) + "=" + rhs.text
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

case class Block(stmts: ListBuffer[Statement], override val textRange: TextRange) extends Expr {
  def text = stmts.map(_.text).mkString("{", "", "}")
}

case class ToBeGiven(typ: Type) extends Expr {
  override def text = ???
  override def textRange = ???
}

case class MethodCall(
  obj: Option[Expr],
  name: String,
  valArgs: ArgList,
  //givenArgs: Seq[Expr], //TODO do givenArgs!!
  typeArgs: Seq[Type]) extends Expr {
  //TODO figure out how to print types
  def text: String = obj.fold("")(_.text + ".") 
    + (if (typeArgs.isEmpty) "" else typeArgs.map(_.text).mkString("<", ",", ">"))
    + name
    + valArgs.text
  override def textRange: TextRange = ???
}

case class ArgList(args: List[Argument], override val textRange: TextRange) extends HasText {
  def text = args.view.map(_.text).mkString("(", ",", ")")
}

object ArgList {
  // given ToJava[ArgList] = argList => argList.args.view.map(_.toJava).mkString("(", ",", ")")
}

case class Argument(expr: Expr) extends HasText {
  def text = expr.text
  def textRange = expr.textRange
}

class FieldAccess(obj: Expr, fieldName: String, fieldNameRange: TextRange) extends Expr, HasText {
  override def textRange = TextRange(obj.textRange.start, fieldNameRange.end)
  def text = s"${obj.text}.$fieldName"
}