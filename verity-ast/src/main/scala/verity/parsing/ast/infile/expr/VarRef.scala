package verity.parsing.ast.infile.expr

import verity.parsing.ast.infile.ValidIdNode

case class VarRef(varName: ValidIdNode) extends Expr {
  override def startOffset = varName.startOffset
  override def endOffset = varName.endOffset
  override def text = varName.text
  override def textRange = varName.textRange
}