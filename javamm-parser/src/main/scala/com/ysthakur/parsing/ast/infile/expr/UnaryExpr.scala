package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing._

sealed abstract class UnaryExpr() extends Expr {
}

case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends UnaryExpr {
//  def startOffset: Int = op.startOffset
//  def endOffset: Int = expr.endOffset
  def text: StringBuilder = new StringBuilder(op.text) append expr.text
}

implicit val ctor: Null = null

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends UnaryExpr {
//  def startOffset: Int = expr.startOffset
//  def endOffset: Int = op.endOffset
  def text: StringBuilder = new StringBuilder(expr.text.toString) append op.text
}