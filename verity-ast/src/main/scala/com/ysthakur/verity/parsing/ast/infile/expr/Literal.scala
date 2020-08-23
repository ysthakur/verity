package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.TextRange

<<<<<<< HEAD
sealed trait Literal extends Expr

enum BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  case TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
  case FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
}


case class NumLiteral(override val text: String, override val textRange: TextRange) extends Literal
=======
trait Literal extends Expr

enum BoolLiteral(override val text: String) extends Literal {
  case TrueLiteral extends BoolLiteral("true")
  case FalseLiteral extends BoolLiteral("false")
}


case class NumLiteral(override val text: String) extends Expr
>>>>>>> master


case class ThisRef(textRange: TextRange) extends Expr {
  override def text: String = "this"
}

case class SuperRef(textRange: TextRange) extends Expr {
  override def text: String = "super"
}