package verity.ast

import verity.ast._
import verity.parsing._
import verity.parsing.GetText.given GetText
import verity.ast.infile._

import scala.collection.mutable.ListBuffer

class FileNode(val packageRef: Option[PackageStmt], val imports: Seq[ImportStmt], val children: Seq[TemplateDef]) {
  def text = s"${packageRef.fold("")(_.text)}${imports.view.map(_.text).mkString}${children.view.map(_.text).mkString}"
}

/**
 * @param path The path of the package this file is in
 * @param pkgTokStartOffset The start offset of the "package" token
 */
class PackageStmt(val path: DotRef, val pkgTokStartOffset: Int) extends Tree, HasText {
  override def text: String = s"package ${path.text};"
  override def textRange = TextRange(pkgTokStartOffset, path.textRange.end)
}

/**
 * @param path The path of the import (excluding the wildcard)
 * @param pkgTokStartOffset The start offset of the "import" token
 */
class ImportStmt(val path: DotRef, val wildCard: Boolean = false, override val textRange: TextRange) extends Tree, HasText {
  override def text = s"import ${path.text}${if (wildCard) ".*" else ""};"
}