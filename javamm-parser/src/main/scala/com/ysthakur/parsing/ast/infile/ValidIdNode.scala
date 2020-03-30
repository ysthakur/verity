package com.ysthakur.parsing.ast.infile

case class ValidIdNode(override val text: String) extends TextNode {
  override def unapply(): (CharSequence, Int, Int) = ???
}
