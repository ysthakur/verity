package com.ysthakur.parsing.ast.infile.expr

case class BinaryExpr(left: Expr, op: Op, right: Expr) extends Expr {
  override def text: String = s"${left.text} ${op.text} ${right.text}"
//  val startOffset = left.startOffset
//  val endOffset = right.endOffset
  //def unapply(): (CharSequence, Int, Int) = ???
}