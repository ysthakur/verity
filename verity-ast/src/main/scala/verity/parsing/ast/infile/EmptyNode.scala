package verity.parsing.ast.infile

import verity.parsing.{TextRange/*, Position*/}

case class EmptyNode(start: Int) extends Node {
  override def text: String = ""
  override def textRange = TextRange.empty(start)
}