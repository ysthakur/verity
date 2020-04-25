package com.ysthakur.javamm.parsing.ast

trait INode {
  def as[T]: T = asInstanceOf[T]
  def flatten: INode = this
}

trait ParentNode extends INode {
  type Child <: INode
  def children: Iterable[Child]
}

trait ChildNode extends INode {
  type Parent <: ParentNode
  def parent: Parent
}
