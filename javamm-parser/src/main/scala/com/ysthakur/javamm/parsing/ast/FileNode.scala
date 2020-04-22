package com.ysthakur.javamm.parsing.ast

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast.Types._

import scala.collection.mutable.ListBuffer

class FileNode(override val parent: PackageNode)
    extends ParentNode
    with ChildNode {
  override type Parent = PackageNode
  override type Child = Field | Method
  override val children: ListBuffer[Child] = ListBuffer()
  def text: String = ???

  //override def unapply(): (CharSequence, Int, Int) = ???
}
