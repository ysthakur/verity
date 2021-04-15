package verity.ast

import verity.ast.*
import verity.parsing.*
import verity.parsing.GetText.given GetText
import verity.ast.infile.*

import com.typesafe.scalalogging.Logger

import scala.collection.mutable.ListBuffer

sealed trait Package {
  def name: String
  def subPkgs: ListBuffer[PkgNode]
  def files: ListBuffer[FileNode]
  override def toString = s"package $name"
}
case class RootPkg(subPkgs: ListBuffer[PkgNode], files: ListBuffer[FileNode]) extends Package {
  def name = "<root>"
}
//TODO does the parent have to be stored?
case class PkgNode(
    name: String,
    subPkgs: ListBuffer[PkgNode],
    files: ListBuffer[FileNode],
    parent: Package
) extends Package

case class FileNode(
    name: String,
    packageRef: Option[PackageStmt],
    imports: Seq[ImportStmt],
    classlikes: Seq[Classlike]
) {
  private[verity] var pkg: Package | Null = null
  def text =
    s"${packageRef.fold("")(_.text)}${imports.view.map(_.text).mkString}${classlikes.view.map(_.text).mkString}"
  override def toString = s"file $name"
}

/** @param path The path of the package this file is in
  * @param pkgTokStartOffset The start offset of the "package" token
  */
case class PackageStmt(val path: DotRef, val pkgTokStartOffset: Int) extends Tree, HasText {
  override def text: String = s"package ${path.text};"
  override def textRange = TextRange(pkgTokStartOffset, path.textRange.end)
}

/** @param path The path of the import (excluding the wildcard)
  * @param pkgTokStartOffset The start offset of the "import" token
  */
case class ImportStmt(path: DotRef, override val textRange: TextRange, wildCard: Boolean = false)
    extends Tree,
      HasText {
  override def text = s"import ${path.text}${if (wildCard) ".*" else ""};"
}
