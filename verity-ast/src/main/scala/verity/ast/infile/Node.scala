package verity.ast.infile

import verity.parsing.{HasText, TextRange}
import verity.ast.Tree

trait Node extends Tree with HasText {
  override def flatten: Node = this
  override def toString = text
  def text: String
  def textRange: TextRange = ??? //TextRange(startOffset, endOffset)
}
