package verity.parsing.ast.infile.expr

import verity.parsing.{HasText, TextRange}
import verity.parsing.ast.infile.Node
//import verity.parsing.lexer._

/**
  * An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(
    //symbol: Token[SymbolTokenType]
    symbol: String, //TODO rectify this!!!
    override val textRange: TextRange
) extends Node {
  def isBinary: Boolean = ???
  override def text: String = symbol //symbol.text
}