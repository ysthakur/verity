package com.ysthakur.javamm.parsing.ast

import com.ysthakur.javamm.parsing.ast.infile.Node

case class ConsNode[N1 <: Node, N2 <: Node](n1: N1, n2: N2) extends Node {
  override def flatten: Node = ConsNode(n1.flatten, n2.flatten)
  override def text: String = s"${n1.text} ${n2.text}"
}