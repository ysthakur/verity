package verity.parsing.ast.infile.expr

import verity.parsing.TextRange

import verity.parsing.TextRange

class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr {
  override def text: String = s"(${left.text} $op ${right.text})"
//  val startOffset = left.startOffset
//  val endOffset = right.endOffset
  //def unapply(): (CharSequence, Int, Int) = ???
  override lazy val textRange = left.textRange to right.textRange
}

// case class AddExpr(left: Expr, right: Expr) extends BinaryExpr(left, right) {

// }