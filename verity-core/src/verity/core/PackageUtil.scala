package verity.core

import verity.ast.{FileNode, Package, Package}

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.mutable

object PackageUtil {
  def walk[T](pkg: Package, f: FileNode => Package ?=> T)(using Package): mutable.Map[FileNode, T] = {
    val results = mutable.Map[FileNode, T]()
    walk(pkg, f, results)
    results
  }

  private def walk[T](pkg: Package, f: FileNode => Package ?=> T, results: mutable.Map[FileNode, T])(using
    root: Package
  ): Unit = {
    //The try-catch is to prevent exceptions from being lost if I ever run these in parallel
    try {
      pkg.subPkgs.foreach { p => walk(p, f, results) }

      pkg.files.foreach(file => results.put(file, f(file)(using root)))
    } catch {
      case e => e.printStackTrace; throw e
    }
  }

  /** Recursively walk down a package and all its subpackages, and for every file encountered, run
    * the given function on that file and the recorded list of packages (reverse order), and the
    * path (as a string).
    * @param f
    *   Execute on a file, also given the list of packages the file is in (in reverse) and the
    *   file's path
    */
  def walkWithPath[T](
    pkg: Package,
    f: (FileNode, List[Package], String) => Package ?=> T
  )(using root: Package): mutable.Map[FileNode, T] = {
    val parents = pkg.parents
    val results = mutable.Map[FileNode, T]()
    walkWithPath(pkg, parents, canonicalName(pkg.name, parents), f, results)(using root)
    results
  }

  private def walkWithPath[T](
    pkg: Package,
    parents: List[Package],
    path: String,
    f: (FileNode, List[Package], String) => Package ?=> T,
    results: mutable.Map[FileNode, T]
  )(using root: Package): Unit = {
    val newParents = pkg :: parents
    val newPath = path + pkg.name

    //The try-catch is to prevent exceptions from being lost if I ever run these in parallel
    try {
      pkg.subPkgs.foreach { p =>
        PackageUtil.walkWithPath(p, newParents, newPath, f, results)(using root)
      }
      pkg.files.foreach(file => results.put(file, f(file, newParents, newPath)))
    } catch {
      case x => x.printStackTrace; throw x
    }
  }

  private def canonicalName(pkgName: String, parents: Iterable[Package]) =
    parents.foldLeft(pkgName)((endName, parentName) => s"$parentName.$endName")
}
