package verity.ast.infile

import verity.parsing.TextRange

case class PackageStmt(dotRef: DotRef, override val textRange: TextRange) extends Node {
  override def text: String = s"package ${dotRef.text};"
}

case class Import(dotRef: DotRef, override val textRange: TextRange, wildCard: Boolean = false) extends Node {
  override def text: String = s"import ${dotRef.text}${if (wildCard) ".*;" else ";"}"
}