package com.ysthakur.javamm.parsing.ast

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast.infile.expr.DotRef
import com.ysthakur.javamm.parsing.ast.infile.{Field, Import, Method, TypeDef}

import scala.collection.mutable.ListBuffer

class FileNode(packageRef: DotRef, imports: ListBuffer[Import], typeDefs: ListBuffer[TypeDef])
    extends ParentNode
        with ChildNode {
  override type Parent = PackageNode
  override type Child = Field | Method
  override val children: ListBuffer[Child] = ListBuffer()
  override def parent: Parent = {
    ???
  }
  def text: String = ???
}
