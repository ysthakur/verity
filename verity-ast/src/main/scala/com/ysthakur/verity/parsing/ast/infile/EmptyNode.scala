package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.TextRange

case class EmptyNode(override val textRange: TextRange) extends Node {
  override def text: String = ""
}