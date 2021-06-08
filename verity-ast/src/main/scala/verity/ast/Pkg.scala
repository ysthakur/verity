package verity.ast

import verity.ast.infile._

import java.io.File
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

sealed trait Pkg extends NamedTree {
  def name: String
  def subPkgs: ArrayBuffer[PkgNode]
  def files: ArrayBuffer[FileNode]

  /** Find a class directly in this package (not in a subpackage) by the given name.
    */
  def findClasslike(className: String): Option[Classlike] =
    this.classlikes.find(_.name == className)

  def classlikes: Iterable[Classlike] = files.flatMap(_.classlikes)

  def parents: List[Pkg]

  override def toString = s"package $name"
}

case class RootPkg(subPkgs: ArrayBuffer[PkgNode], files: ArrayBuffer[FileNode]) extends Pkg {
  def name = ""
  def parents = Nil
}

//TODO does the parent have to be stored?
case class PkgNode(
  name: String,
  subPkgs: ArrayBuffer[PkgNode],
  files: ArrayBuffer[FileNode],
  parent: Pkg
) extends Pkg {
  def parents = parent :: parent.parents
}

object Pkg {
  type Importable = (Pkg | Classlike | Field | MethodGroup | EnumConstant) & NamedTree
  type ImportParent = Pkg | Classlike

  /** Find a subpackage given the relative path of the package. The last found subpackage and the
    * remaining path are returned.
    * @param pkgPath
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  @tailrec def findPkgRel(
    pkg: Pkg,
    pkgPath: Iterable[String]
  ): (Pkg, Iterable[String]) =
    if pkgPath.isEmpty then {
      (pkg, pkgPath)
    } else {
      val subName = pkgPath.head

      pkg.subPkgs.find(_.name == subName) match {
        case Some(subPkg) => findPkgRel(subPkg, pkgPath.tail)
        case None         => (pkg, pkgPath)
      }
    }

  /** Find a subpackage/class/method/field given the relative path of the tree.
    * @param path
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  def findImptableRel(pkg: Pkg, path: Iterable[String]): Option[Importable] =
    if path.isEmpty then {
      Some(pkg)
    } else {
      val subName = path.head
      val rest = path.tail

      pkg.subPkgs.view
        .find(_.name == subName)
        .flatMap(findImptableRel(_, rest))
        .asInstanceOf[Option[Pkg.Importable]]
        .orElse(
          findCls(pkg, subName).flatMap(cls =>
            if (rest.isEmpty) Some(cls)
            else cls.findMember(rest).asInstanceOf[Option[Pkg.Importable]]
          ): Option[Pkg.Importable]
        ): Option[Pkg.Importable]
    }

  /** Find a subpackage given the absolute path of the package. The last found subpackage and the
    * remaining path are returned.
    * @param pkgPath
    *   The path of a subpackage or class or other tree somewhere inside this package. The Iterable
    *   is *not* in reverse order.
    */
  def findPkgPAbs(pkgPath: Iterable[String])(using root: RootPkg): (Pkg, Iterable[String]) =
    findPkgRel(root, pkgPath)

  def findImptableAbs(path: Iterable[String])(using root: RootPkg): Option[Importable] =
    findImptableRel(root, path)

  /** Find a class with the given name directly in this package (not in a subpackage)
    */
  def findCls(pkg: Pkg, clsName: String): Option[Classlike] =
    pkg.classlikes.find(_.name == clsName)
}
