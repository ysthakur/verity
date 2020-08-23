package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing.HasText
import com.ysthakur.verity.parsing.ast.infile.Node
//import com.ysthakur.verity.parsing.lexer._

/**
  * An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(
    //symbol: Token[SymbolTokenType]
    symbol: String //TODO rectify this!!!
) extends Node {
  def isBinary: Boolean = ???
  override def text: String = symbol //symbol.text
}