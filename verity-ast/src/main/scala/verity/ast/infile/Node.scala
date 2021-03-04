package verity.ast.infile

import verity.parsing.{HasText, TextRange}
import verity.ast.INode

trait Node extends INode with HasText {
  override def flatten: Node = this
  def text: String
  def textRange: TextRange = TextRange(startOffset, endOffset)
}
