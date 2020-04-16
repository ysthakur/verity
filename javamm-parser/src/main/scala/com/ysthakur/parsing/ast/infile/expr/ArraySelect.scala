package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.ast.Types._

case class ArraySelect(arr: Expr, index: Expr) extends Expr {
  def this(arr: Node, index: Node) = this(arr.as[Expr], index.as[Expr])
}