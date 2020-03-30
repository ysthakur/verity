package com.ysthakur.parsing.ast

trait Node

trait ParentNode extends Node {
  def children: Iterable[Node]
}

trait ChildNode extends Node {
  type Parent <: ParentNode
  def parent: Parent
}
