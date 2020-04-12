package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.infile.TextNode
import com.ysthakur.parsing.lexer._

/**
  * An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(
    symbol: Token[SymbolTokenType]
) extends TextNode {
  def isBinary: Boolean = ???
  override def text: String = symbol.text
  override def startOffset: Int = symbol.startOffset
  override def endOffset: Int = symbol.endOffset
}
