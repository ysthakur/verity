package com.ysthakur.verity.parsing.ast

import com.ysthakur.verity.parsing.ast.infile.{EmptyNode, Node}

case class ConsNode[N1 <: Node, N2 <: Node](n1: N1|EmptyNode.type, n2: N2|EmptyNode.type) extends Node {

  //println(s"\n______________________________\nCreated consnode, n1=$n1, n2=$n2")
  override def flatten: Node = ConsNode(n1.flatten, n2.flatten)
  override def text: String = s"${n1.text} ${n2.text}"
}