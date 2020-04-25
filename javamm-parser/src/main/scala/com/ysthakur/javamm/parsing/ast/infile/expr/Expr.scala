package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast.infile.{Node, TypeRepr}

trait Expr extends Node {
  var _exprType: TypeRepr|Null = _
  def exprType = _exprType
  def exprType_=(typeRepr: TypeRepr) = _exprType = typeRepr
}