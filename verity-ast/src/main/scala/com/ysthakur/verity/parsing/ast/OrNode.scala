package com.ysthakur.verity.parsing.ast

import com.ysthakur.verity.parsing.ast.infile.Node

sealed trait OrNode[L <: INode, R <: INode](node: L|R) extends INode {
  // override def text: String = node.text
}

case class LeftNode[L <: INode, R <: INode](left: L) extends OrNode[L, R](left) {
  // override def flatten: Node = left match {
  //   case orNode: OrNode[?, ?] => orNode.flatten
  //   case _ => left
  // }
}

case class RightNode[L <: INode, R <: INode](right: R) extends OrNode[L, R](right) {
  // override def flatten: Node = right match {
  //   case orNode: OrNode[?, ?] => orNode.flatten
  //   case _ => right
  // }
}