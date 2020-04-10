package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.infile.TextNode
import com.ysthakur.parsing.lexer._

case class Operator(
    symbol: SymbolTokenType,
    override val startOffset: Int,
    override val endOffset: Int
) extends TextNode {
  def isBinary: Boolean = ???
  override def text: String = symbol.text
}
