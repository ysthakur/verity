package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast.Node

trait TextNode extends Node with HasText {
  override def flatten: TextNode = this
  def text: String
}
