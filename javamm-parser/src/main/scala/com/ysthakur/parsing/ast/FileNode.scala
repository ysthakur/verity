package com.ysthakur.parsing.ast

import com.ysthakur.parsing.HasText

class FileNode(override val parent: PackageNode)
    extends ParentNode
    with ChildNode
    with HasText {
  type Parent = PackageNode
  override def children: Iterable[Node with HasText] = ???
  override val startOffset: Int = 0
  override lazy val endOffset = text.length
  override lazy val text: String = ???

  //override def unapply(): (CharSequence, Int, Int) = ???
}
