package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing._

<<<<<<< HEAD:verity-ast/src/main/scala/com/ysthakur/verity/parsing/ast/infile/expr/UnaryExpr.scala
sealed trait UnaryExpr[+E <: Expr](expr: E) extends Expr
=======
sealed trait UnaryExpr[+E <: Expr](expr: E) extends Expr {
}
>>>>>>> master:javamm-ast/src/main/scala/com/ysthakur/javamm/parsing/ast/infile/expr/UnaryExpr.scala

case class UnaryPreExpr[O <: Op, +E <: Expr](op: Op, expr: E) extends UnaryExpr[E](expr) {
//  def startOffset: Int = op.startOffset
//  def endOffset: Int = expr.endOffset
  override def text: String = s"(${op.text} ${expr.text})"
  override lazy val textRange = op.textRange to expr.textRange
}

case class UnaryPostExpr[E <: Expr, O <: Op](expr: E, op: O) extends UnaryExpr[E](expr) {
//  def startOffset: Int = expr.startOffset
//  def endOffset: Int = op.endOffset
  override def text: String = s"(${expr.text}${op.text})"
  override lazy val textRange = expr.textRange to op.textRange
}