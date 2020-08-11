package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.{HasText, TextRange}
import com.ysthakur.verity.parsing.ast.INode

trait Node extends INode with HasText {
  override def flatten: Node = this
  def text: String
  def textRange: TextRange
}