package verity.ast.infile

import verity.parsing.TextRange

case class ParamList(override val textRange: TextRange) extends Node {
  override def text: String = ???
}
