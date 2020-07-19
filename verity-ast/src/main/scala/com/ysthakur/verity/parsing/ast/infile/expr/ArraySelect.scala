package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.ast.infile.Node

case class ArraySelect(arr: Expr, index: Expr) extends Expr {
  def this(arr: Node, index: Node) = this(arr.as[Expr], index.as[Expr])
  override def text: String = s"${arr.text}[${index.text}]"
}