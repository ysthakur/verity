package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.TextRange

trait Literal extends Expr

enum BoolLiteral(override val text: String) extends Literal {
  case TrueLiteral extends BoolLiteral("true")
  case FalseLiteral extends BoolLiteral("false")
}


case class NumLiteral(override val text: String) extends Expr


case class ThisRef(textRange: TextRange) extends Expr {
  override def text: String = "this"
}

case class SuperRef(textRange: TextRange) extends Expr {
  override def text: String = "super"
}