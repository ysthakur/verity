package verity.parsing.ast.infile

import verity.parsing.TextRange
import verity.parsing.ast.infile.expr.Expr

case class ValidIdNode(name: String, override val textRange: TextRange) extends Expr {
  // def unapply(): (CharSequence, Int, Int) = ???
//  def startOffset: Int = ???
//  def endOffset: Int = ???
  override def text: String = name
}