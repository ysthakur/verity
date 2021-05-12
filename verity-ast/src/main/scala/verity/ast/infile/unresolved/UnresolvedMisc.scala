package verity.ast.infile.unresolved

import verity.ast.{Tree, Text, TextRange, HasText, HasTextRange}
import verity.ast.infile.{Statement, ResolvedOrUnresolvedExpr}

/**
 * An unresolved expression with a semicolon after it
 */
class UnresolvedExprStmt(val expr: ResolvedOrUnresolvedExpr) extends Statement {
  override def text = s"${expr.text};"
  override def textRange: TextRange = expr.textRange
}

/** For something like `foo.bar.baz`, as long as it comes before a method call such as
  * `foo.bar.baz.blah(bleh)`.
  */
case class MultiDotRef(path: Seq[Text]) extends HasTextRange {
  override def text: String = path.view.map(_.text).mkString(".")
  override def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

class UnresolvedError(elem: Tree, msg: String) extends Exception(msg)
