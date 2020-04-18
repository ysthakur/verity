package com.ysthakur.parsing.ast

import com.ysthakur.parsing.ast.infile.TextNode

case class ConsNode[N1 <: TextNode, N2 <: TextNode](n1: N1, n2: N2) extends TextNode {
  override def flatten: TextNode = ConsNode(n1.flatten, n2.flatten)
  override def text: String = s"${n1.text} ${n2.text}"
}