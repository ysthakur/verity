package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast.INode

trait Node extends INode with HasText {
  override def flatten: Node = this
  def text: String
}
