package verity.ast

import verity.ast._
import verity.parsing._
import verity.parsing.GetText.given GetText
import verity.ast.infile._

import scala.collection.mutable.ListBuffer

case class FileNode(packageRef: Option[PackageStmt], imports: Seq[ImportStmt], children: Seq[TemplateDef]) {
  def text = s"${packageRef.fold("")(_.text)}${imports.view.map(_.text).mkString}${children.view.map(_.text).mkString}"
}

/**
 * @param path The path of the package this file is in
 * @param pkgTokStartOffset The start offset of the "package" token
 */
case class PackageStmt(val path: DotRef, val pkgTokStartOffset: Int) extends Tree, HasText {
  override def text: String = s"package ${path.text};"
  override def textRange = TextRange(pkgTokStartOffset, path.textRange.end)
}

/**
 * @param path The path of the import (excluding the wildcard)
 * @param pkgTokStartOffset The start offset of the "import" token
 */
case class ImportStmt(path: DotRef, override val textRange: TextRange, wildCard: Boolean = false) extends Tree, HasText {
  override def text = s"import ${path.text}${if (wildCard) ".*" else ""};"
}