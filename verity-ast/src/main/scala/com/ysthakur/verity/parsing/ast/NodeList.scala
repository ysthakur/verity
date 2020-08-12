package com.ysthakur.verity.parsing.ast

import com.ysthakur.verity.parsing.{HasText, TextRange}
import com.ysthakur.verity.parsing.ast.infile.Node

case class NodeList[+T <: Node](val nodes: List[T], override val textRange: TextRange) extends Node {

  //println(s"\n----------------------------\nCreated nodelist, nodes=$nodes\n")

  // override def flatten: NodeList[?] = NodeList(nodes.map(_.flatten))
  override def toString: String = s"NodeList(${nodes.toString})"
  override def equals(other: Any): Boolean = other match {
    case list: NodeList[?] => nodes == list.nodes
    case _ => false
  }
  override def text: String = nodes.map(_.text).mkString
}