package verity.ast.infile

import verity._
import verity.ast._

//TODO add other kinds of statements
trait Statement extends Tree, HasText {
}

/**
 * An expression with a semicolon after it
 */
class ExprStmt(val expr: Expr, end: Int) extends Statement {
  override def text = s"${expr.text};"
  override def textRange = TextRange(expr.textRange.start, end)
}

class ReturnStmt(val expr: ResolvedOrUnresolvedExpr, val textRange: TextRange) extends Statement {
  override def text = s"return ${expr.text};"
}
