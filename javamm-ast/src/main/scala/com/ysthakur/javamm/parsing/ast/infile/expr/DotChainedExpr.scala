package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.ast.infile.ValidIdNode

case class DotChainedExpr(expr: Expr, validId: ValidIdNode) extends Expr {
  override def text: String = s"${expr.text}.${validId.text}"
}