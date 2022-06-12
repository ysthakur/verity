package verity.ast

import java.io.File
import scala.collection.mutable

case class FileNode(
  name: String,
  packageRef: Option[PackageStmt],
  imports: Seq[ImportStmt],
  origFile: Option[File],
  offsetToPos: Iterable[(Int, Int, Int)]
) {
  val textRanges: mutable.Map[Tree, TextRange] = mutable.HashMap()
  private[verity] var pkg: Package | Null = null
  private[verity] var resolvedImports: Iterable[Def] = List.empty

  def getPos(targetOffset: Int): Position = FileNode.getPos(offsetToPos, targetOffset)

  /** Whether or not this is a source file or just a classfile that the project depends on.
    */
  def isSource: Boolean = origFile.nonEmpty

  override def toString = s"file $name"
}

object FileNode {
  def getPos(offsetToPos: Iterable[(Int, Int, Int)], targetOffset: Int): Position = {
    if (targetOffset < 0) {
      Position.synthetic
    } else {
      val prevRow = 1
      val prevCol = 0
      val it = offsetToPos.iterator
      var shouldBreak = false

      while (it.hasNext && !shouldBreak) {
        val (offset, row, endCol) = it.next
        if (targetOffset <= offset) {}
      }
      offsetToPos
        .find { case (offset, _, _) =>
          targetOffset <= offset
        }
        .getOrElse(offsetToPos.last) match {
        case (offset, row, endCol) =>
          Position(row, endCol - (offset - targetOffset) + 1, targetOffset)
      }
    }
  }
}

/** @param path
  *   The path of the package this file is in
  */
case class PackageStmt(val path: Iterable[String]) extends Tree

/** @param path
  *   The path of the import (excluding the wildcard)
  */
case class ImportStmt(path: Iterable[String], wildCard: Boolean = false) extends Tree
