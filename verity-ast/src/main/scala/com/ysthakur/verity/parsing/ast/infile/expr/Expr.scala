package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.{HasText, TextRange}
import com.ysthakur.verity.parsing.ast.infile.{Node, TypeRepr}

trait Expr extends Node {
  var _exprType: TypeRepr|Null = _
  def exprType = _exprType
  def exprType_=(typeRepr: TypeRepr) = _exprType = typeRepr
}

case class ParenExpr(expr: Expr, tr: TextRange) extends Expr {
  def text = s"(${expr.text})"
}

case class ArraySelect(arr: Expr, index: Expr) extends Expr {
  def this(arr: Node, index: Node) = this(arr.as[Expr], index.as[Expr])
  override def text: String = s"${arr.text}[${index.text}]"
}
