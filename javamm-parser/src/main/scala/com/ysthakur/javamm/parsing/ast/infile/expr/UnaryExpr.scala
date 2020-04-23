package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing._

sealed abstract class UnaryExpr() extends Expr {
}

case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends UnaryExpr {
//  def startOffset: Int = op.startOffset
//  def endOffset: Int = expr.endOffset
  override def text: String = s"(${op.text} ${expr.text})"
}

implicit val ctor: Null = null

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends UnaryExpr {
//  def startOffset: Int = expr.startOffset
//  def endOffset: Int = op.endOffset
  override def text: String = s"(${expr.text}${op.text})"
}