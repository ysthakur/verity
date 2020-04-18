package com.ysthakur.parsing.ast

import com.ysthakur.parsing.ast.infile.TextNode

sealed trait OrNode[L <: TextNode, R <: TextNode] extends TextNode

case class LeftNode[L <: TextNode, R <: TextNode](left: L) extends OrNode[L, R] {
  override def flatten: L = left
  override def text: String = ???
}
case class RightNode[L <: TextNode, R <: TextNode](right: R) extends OrNode[L, R] {
  override def flatten: R = right
  override def text: String = ???
}