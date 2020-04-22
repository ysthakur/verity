package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.ast.Types._

case class ArraySelect(arr: Expr, index: Expr) extends Expr {
  def this(arr: Node, index: Node) = this(arr.as[Expr], index.as[Expr])
  override def text: String = s"${arr.text}[${index.text}]"
}