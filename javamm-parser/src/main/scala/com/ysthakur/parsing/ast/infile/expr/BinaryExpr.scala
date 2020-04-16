package com.ysthakur.parsing.ast.infile.expr

case class BinaryExpr(left: Expr, op: Op, right: Expr) extends Expr {
  override def text: StringBuilder = 
    StringBuilder(left.text.toString) append op.text append right.text
//  val startOffset = left.startOffset
//  val endOffset = right.endOffset
  //def unapply(): (CharSequence, Int, Int) = ???
}