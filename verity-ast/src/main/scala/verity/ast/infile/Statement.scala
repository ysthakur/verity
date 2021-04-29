package verity.ast.infile

import verity.*
import verity.ast.*

//TODO add other kinds of statements
trait Statement extends Tree, HasText {
}

/**
 * An expression with a semicolon after it
 */
class ExprStmt(val expr: Expr, end: Int) extends Statement {
  def text = s"${expr.text};"
  def textRange = TextRange(expr.textRange.start, end)
}

class ReturnStmt(val expr: Expr, val textRange: TextRange) extends Statement {
  def text = s"return ${expr.text};"
}