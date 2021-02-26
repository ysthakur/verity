package verity.parsing.ast.infile

import verity.parsing.{HasText, TextRange}
import verity.parsing.ast.INode

trait Node extends INode with HasText {
  override def flatten: Node = this
  def text: String
  def textRange: TextRange = TextRange(startOffset, endOffset)
}
