package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.expr.DotRef

case class PackageStmt(dotRef: DotRef, override val textRange: TextRange) extends Node {
  override def text: String = s"package ${dotRef.text};"
}

case class Import(dotRef: DotRef, override val textRange: TextRange, wildCard: Boolean = false) extends Node {
  override def text: String = s"import ${dotRef.text}${if (wildCard) ".*;" else ";"}"
}