package com.ysthakur.parsing.ast.infile

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast.infile.expr.DotRef

case class PackageStmt(dotRef: DotRef) extends TextNode {
  override def text: String = s"package ${dotRef.text};"
}

case class Import(dotRef: DotRef, wildCard: Boolean = false) extends TextNode {
  override def text: String = s"import ${dotRef.text}${if (wildCard) ".*;" else ";"}"
}