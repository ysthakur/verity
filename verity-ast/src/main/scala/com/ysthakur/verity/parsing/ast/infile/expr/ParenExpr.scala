package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing._

case class ParenExpr(expr: Expr, tr: TextRange) extends Expr {
  def text = s"(${expr.text})"
}