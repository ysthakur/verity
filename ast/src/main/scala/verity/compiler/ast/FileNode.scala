import java.io.File
import scala.collection.mutable

import cats.data.NonEmptyList

case class FileNode(
  name: String,
  packageRef: Option[PackageStmt],
  imports: Seq[ImportStmt],
  typedefs: List[TypeDef],
  origFile: Option[File]
) {
  private[verity] var resolvedImports: Iterable[Def] = List.empty

  /** Whether or not this is a source file or just a classfile that the project
    * depends on.
    */
  def isSource: Boolean = origFile.nonEmpty

  override def toString = s"file $name"
}

/** @param path
  *   The path of the package this file is in
  */
case class PackageStmt(val path: NonEmptyList[String]) extends Tree

/** @param path
  *   The path of the import, excluding the wildcard
  */
case class ImportStmt(
  path: NonEmptyList[String],
  wildcard: Boolean = false
) extends Tree
