package com.ysthakur.javamm.parsing.ast

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast.infile.expr.DotRef
import com.ysthakur.javamm.parsing.ast.infile.{Field, Import, Method, TypeDef}

import scala.collection.mutable.ListBuffer

class FileNode(packageRef: DotRef, imports: ListBuffer[Import], override val children: ListBuffer[TypeDef])
    extends ParentNode
        with ChildNode {
  override type Parent = PackageNode
  override type Child = TypeDef
  override def parent: Parent = {
    ???
  }
  def text: String = ???
}
