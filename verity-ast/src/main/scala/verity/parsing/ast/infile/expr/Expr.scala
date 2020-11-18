package verity.parsing.ast.infile.expr

import verity.parsing.{Token, HasText, TextRange}
import verity.parsing.ast.infile.{Node, TypeRepr, ValidId}

trait Expr extends Node {
  var _exprType: TypeRepr|Null = _
  def exprType = _exprType
  def exprType_=(typeRepr: TypeRepr) = _exprType = typeRepr
}

/**
 * Can be on the left hand side of an assignment expression
 */
trait Assignable extends Expr {
  
}

case class VarRef(varName: ValidId) extends Expr {
  override def startOffset = varName.startOffset
  override def endOffset = varName.endOffset
  override def text = varName.text
  override def textRange = varName.textRange
}

case class ParenExpr(expr: Expr, override val textRange: TextRange) extends Expr {
  def text = s"(${expr.text})"
}

case class DotChainedExpr(expr: Expr, propertyName: ValidId) extends Expr {
  override def text: String = s"${expr.text}.${propertyName.text}"
  override lazy val textRange = expr.textRange to propertyName.textRange
}

case class ArraySelect(arr: Expr, index: Expr, override val textRange: TextRange) extends Expr {
  override def text: String = s"${arr.text}[${index.text}]"
}

class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr {
  override def text: String = s"(${left.text} $op ${right.text})"
  //  val startOffset = left.startOffset
  //  val endOffset = right.endOffset
  //def unapply(): (CharSequence, Int, Int) = ???
  override lazy val textRange = left.textRange to right.textRange
}


case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends Expr {
  //  def startOffset: Int = op.startOffset
  //  def endOffset: Int = expr.endOffset
  override def text: String = s"(${op.text} ${expr.text})"
  override lazy val textRange = op.textRange to expr.textRange
}

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends Expr {
  //  def startOffset: Int = expr.startOffset
  //  def endOffset: Int = op.endOffset
  override def text: String = s"(${expr.text}${op.text})"
  override lazy val textRange = expr.textRange to op.textRange
}

case class AssignmentExpr(lhs: Assignable, rhs: Expr, extraOp: Token | Null) extends Expr {
  def text = lhs.text + (if (extraOp == null) "" else extraOp.text) + "=" + rhs.text
  def textRange = TextRange(lhs.textRange.start, rhs.textRange.end)
}

/**
 * An operator
 * @param symbol
 * @param startOffset
 * @param endOffset
 */
case class Op(
               //symbol: Token[SymbolTokenType]
               symbol: String, //TODO rectify this!!!
               override val textRange: TextRange
             ) extends Node {
  def isBinary: Boolean = ???
  override def text: String = symbol //symbol.text
}