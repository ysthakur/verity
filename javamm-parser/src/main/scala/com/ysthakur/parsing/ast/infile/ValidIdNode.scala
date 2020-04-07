package com.ysthakur.parsing.ast.infile

case class ValidIdNode(
  override val text: String, 
  override val startOffset: Int, 
  override val endOffset: Int) extends TextNode {
  // def unapply(): (CharSequence, Int, Int) = ???
}
