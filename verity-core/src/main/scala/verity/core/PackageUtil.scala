package verity.core

import verity.ast.{FileNode, Pkg, RootPkg}
import com.typesafe.scalalogging.Logger

import scala.concurrent.{ExecutionContext, Future}

object PackageUtil {
  def walk(pkg: Pkg, f: FileNode => (RootPkg, Logger) ?=> Unit)(using RootPkg, Logger): Unit = {
    pkg.subPkgs.foreach { p => walk(p, f) }

    pkg.files.foreach(f)
  }

  /** Recursively walk down a package and all its subpackages, and for every file encountered, run
    * the given function on that file and the recorded list of packages (reverse order), and the
    * path (as a string).
    * @param f
    *   Execute on a file, also given the list of packages the file is in (in reverse) and the
    *   file's path
    */
  def walkWithPath(
    pkg: Pkg,
    f: (FileNode, List[Pkg], String) => (RootPkg, Logger) ?=> Unit
  )(using root: RootPkg, logger: Logger): Unit = {
    val parents = pkg.parents
    walkWithPath(pkg, parents, canonicalName(pkg.name, parents), f)(using root, logger)
  }

  private def walkWithPath(
    pkg: Pkg,
    parents: List[Pkg],
    path: String,
    f: (FileNode, List[Pkg], String) => (RootPkg, Logger) ?=> Unit
  )(using root: RootPkg, logger: Logger): Unit = {
    val newParents = pkg :: parents
    val newPath = path + pkg.name
    pkg.subPkgs.foreach { p =>
      PackageUtil.walkWithPath(p, newParents, newPath, f)(using root, logger)
    }
    try {
      pkg.files.foreach(f(_, newParents, newPath))
    } catch {
      case x => x.printStackTrace; throw x
    }
  }

  private def canonicalName(pkgName: String, parents: Iterable[Pkg]) =
    parents.foldLeft(pkgName)((endName, parentName) => s"$parentName.$endName")
}
