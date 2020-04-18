package com.ysthakur.parsing.ast

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast.infile.TextNode

class NodeList[+T <: TextNode](val nodes: Iterable[T]) extends TextNode {
  override def flatten: NodeList[?] = NodeList(nodes.map(_.flatten))
  override def toString: String = s"NodeList(${nodes.toString})"
  override def equals(other: Any): Boolean = other match {
    case list: NodeList[?] => nodes == list.nodes
    case _ => false
  }
  override def text: String = nodes.map(_.text).mkString
}