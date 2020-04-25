package com.ysthakur.javamm.parsing.ast

import com.ysthakur.javamm.parsing.ast.infile.Node

sealed trait OrNode[L <: Node, R <: Node](node: L|R) extends Node {
  override def text: String = node.text
}

case class LeftNode[L <: Node, R <: Node](left: L) extends OrNode[L, R](left) {
  override def flatten: Node = left match {
    case orNode: OrNode[?, ?] => orNode.flatten
    case _ => left
  }
}

case class RightNode[L <: Node, R <: Node](right: R) extends OrNode[L, R](right) {
  override def flatten: Node = right match {
    case orNode: OrNode[?, ?] => orNode.flatten
    case _ => right
  }
}