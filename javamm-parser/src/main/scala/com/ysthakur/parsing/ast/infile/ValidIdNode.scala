package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.lexer.{Token, ValidIdentifierTokenType, VariantToken}

case class ValidIdNode(token: Token[ValidIdentifierTokenType] /*, 
  override val startOffset: Int, 
  override val endOffset: Int*/) extends TextNode {
  // def unapply(): (CharSequence, Int, Int) = ???
//  def startOffset: Int = ???
//  def endOffset: Int = ???
  override def text: String = token.text
}