package com.ysthakur.parsing.ast.infile.expr

case class BinaryExpr(left: Expr, op: Operator, right: Expr) extends Expr {
  def text: String = ???
  def unapply(): (CharSequence, Int, Int) = ???
}
