package verity.ast

import verity.*
import verity.ast.*

//TODO add other kinds of statements
trait Statement extends Tree, HasTextRange {}

/** An expression with a semicolon after it
  */
class ExprStmt(val expr: Expr) extends Statement {
  override def text = s"${expr.text};"
  override def textRange = expr.textRange
}

class ReturnStmt(val expr: Option[ResolvedOrUnresolvedExpr], override val textRange: TextRange)
    extends Statement {
  override def text = s"return ${expr.fold("")(_.text)};"
}
