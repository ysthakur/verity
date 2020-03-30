package com.ysthakur.parsing.ast

case class PackageNode() extends ParentNode {
  override lazy val children: Iterable[Node] = ???
}

abstract class Directory extends Node
