package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.TextRange

sealed trait Literal extends Expr

enum BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  case TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
  case FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
}


case class NumLiteral(override val text: String, override val textRange: TextRange) extends Literal

case class ThisRef(textRange: TextRange) extends Expr {
  override def text: String = "this"
}

case class SuperRef(textRange: TextRange) extends Expr {
  override def text: String = "super"
}