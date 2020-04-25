package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.TextRange

case class ThisRef(textRange: TextRange) extends Expr {
  override def text: String = "this"
}
case class SuperRef(textRange: TextRange) extends Expr {
  override def text: String = "super"
}
