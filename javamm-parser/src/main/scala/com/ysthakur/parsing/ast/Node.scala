package com.ysthakur.parsing.ast

trait Node {
  def as[T]: T = asInstanceOf[T]
  def flatten: Node = this
}

trait ParentNode extends Node {
  type Child <: Node
  def children: Iterable[Child]
}

trait ChildNode extends Node {
  type Parent <: ParentNode
  def parent: Parent
}
