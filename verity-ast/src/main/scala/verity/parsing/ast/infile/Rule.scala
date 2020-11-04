package verity.parsing.ast.infile

import verity.parsing.TextRange

case class Rule(params: ParamList, override val textRange: TextRange) extends Node {
  override def text: String = s"rule${params.text}" //TODO implement this properly
}
