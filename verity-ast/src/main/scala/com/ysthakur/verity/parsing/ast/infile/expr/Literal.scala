package com.ysthakur.verity.parsing.ast.infile.expr

trait Literal extends Expr {

}

sealed trait BooleanLiteral extends Literal
object TrueLiteral extends BooleanLiteral {
  override def text: String = "true"
}
object FalseLiteral extends BooleanLiteral {
  override def text: String = "false"
}
