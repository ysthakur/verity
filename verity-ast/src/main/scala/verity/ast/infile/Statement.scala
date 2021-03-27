package verity.ast.infile

import verity._
import verity.ast._
import verity.parsing._

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