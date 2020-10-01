package com.ysthakur.verity.parsing.ast

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.{EmptyNode, Node}

case class ConsNode[N1 <: Node, N2 <: Node](n1: N1, n2: N2) extends Node {

  //println(s"\n______________________________\nCreated consnode, n1=$n1, n2=$n2")
  // override def flatten: Node = ConsNode(n1.flatten, n2.flatten)
  override def text: String = s"${n1.text}${n2.text}"
  override def textRange = TextRange(n1.textRange.start, n2.textRange.end)
}