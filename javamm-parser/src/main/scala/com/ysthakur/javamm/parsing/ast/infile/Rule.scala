package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

case class Rule(params: ParamList) extends Node {
  override def text: String = s"rule${params.text}" //TODO implement this properly
}
