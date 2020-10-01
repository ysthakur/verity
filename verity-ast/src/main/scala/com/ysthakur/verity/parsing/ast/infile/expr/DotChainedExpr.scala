package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.ValidIdNode

case class DotChainedExpr(expr: Expr, propertyName: ValidIdNode) extends Expr {
  override def text: String = s"${expr.text}.${propertyName.text}"
  override lazy val textRange = expr.textRange to propertyName.textRange
}