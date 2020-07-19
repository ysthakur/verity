package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing._

sealed trait UnaryExpr[+E <: Expr](expr: E) extends Expr {
}

case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends UnaryExpr[E](expr) {
//  def startOffset: Int = op.startOffset
//  def endOffset: Int = expr.endOffset
  override def text: String = s"(${op.text} ${expr.text})"
}

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends UnaryExpr[E](expr) {
//  def startOffset: Int = expr.startOffset
//  def endOffset: Int = op.endOffset
  override def text: String = s"(${expr.text}${op.text})"
}