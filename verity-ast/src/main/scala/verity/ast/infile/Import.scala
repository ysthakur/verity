package verity.ast.infile

import verity.ast.Tree
import verity.parsing._

case class PackageStmt(dotRef: DotRef, override val textRange: TextRange) extends Tree, HasText {
  override def text: String = s"package ${dotRef.text};"
}

case class Import(dotRef: DotRef, wildCard: Boolean = false, override val textRange: TextRange) extends Tree, HasText {
  override def text = s"import ${dotRef.text}${if (wildCard) ".*" else ""};"
}