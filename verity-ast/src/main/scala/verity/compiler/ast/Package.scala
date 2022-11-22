package verity.compiler.ast

import java.io.File
import scala.annotation.{tailrec, targetName}

case class Package(
  name: String,
  children: List[Package],
  files: List[FileNode]
) extends Def {
  /** Find a child package given its relative path */
  def findChild(path: String*): Option[Package] = {
    val (child, remaining) = Package.findPkgRel(this, path)
    Option.when(remaining.isEmpty)(child)
  }

  /** Find a typedef directly inside this package */
  def findType(name: String): Option[TypeDef] = files.view.flatMap(_.typedefs).find(_.name == name)
}

object Package {
  /** Find a subpackage given the relative path of the package. The last found subpackage and the
    * remaining path are returned.
    * @param pkgPath
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  @tailrec private def findPkgRel(
    pkg: Package,
    pkgPath: Iterable[String]
  ): (Package, Iterable[String]) =
    if (pkgPath.isEmpty) {
      (pkg, pkgPath)
    } else {
      val subName = pkgPath.head

      pkg.children.find(_.name == subName) match {
        case Some(subPkg) => findPkgRel(subPkg, pkgPath.tail)
        case None => (pkg, pkgPath)
      }
    }

  /** Find a subpackage/class/method/field given the relative path of the tree.
    * @param path
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  def findImptableRel(pkg: Package, path: Iterable[String]): Option[Def] =
    if (path.isEmpty) {
      Some(pkg)
    } else {
      val subName = path.head
      val rest = path.tail

      pkg.children.view
        .find(_.name == subName)
        .flatMap(findImptableRel(_, rest))
    }

  /** Find a subpackage given the absolute path of the package. The last found subpackage and the
    * remaining path are returned.
    * @param pkgPath
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  def findPkgPAbs(pkgPath: Iterable[String])(using root: Package): (Package, Iterable[String]) =
    findPkgRel(root, pkgPath)

  def findImptableAbs(path: Iterable[String])(using root: Package): Option[Def] =
    findImptableRel(root, path)
}
