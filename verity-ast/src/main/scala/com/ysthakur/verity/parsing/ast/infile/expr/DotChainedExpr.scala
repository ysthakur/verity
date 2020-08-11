package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.ValidIdNode

case class DotChainedExpr(expr: Expr, validId: ValidIdNode, override val textRange: TextRange) extends Expr {
  override def text: String = s"${expr.text}.${validId.text}"
}