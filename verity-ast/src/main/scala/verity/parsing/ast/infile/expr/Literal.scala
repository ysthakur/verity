package verity.parsing.ast.infile.expr

import verity.parsing.{Token, TokenType, TextRange}

sealed trait Literal extends Expr

enum BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  case TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
  case FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
}
object BoolLiteral {
  def apply(token: Token) =
    if (token.text.charAt(0) == 't') TrueLiteral(token.textRange)
    else FalseLiteral(token.textRange)
}


case class NumLiteral(override val text: String, override val textRange: TextRange) extends Literal

enum NumType {
  case INT
  case LONG
  case FLOAT
  case DOUBLE
}

case class ThisRef(textRange: TextRange) extends Expr {
  override def text: String = "this"
}

case class SuperRef(textRange: TextRange) extends Expr {
  override def text: String = "super"
}