package verity.core.references

import verity.ast.*
import verity.ast.infile.*
//import verity.util.*

import com.typesafe.scalalogging.Logger

object ReferenceResolve {
  /**
 * Resolve all references to classes and type parameters in a package
 * @param pkg The package to work on
 * @param parentPkgs A list of this package's parents (topmost packages at the end)
 * @param logger The logger to use
 */
  def resolveSimpleReferencesInPkg(
    pkg: Package,
    parentPkgs: List[Package],
    pkgName: String
  )(using root: RootPkg, logger: Logger): Unit = {
    val currParentPkgs = pkg :: parentPkgs
    var currPkgName = s"$pkgName.${pkg.name}"
    pkg.files.foreach(resolveSimpleReferencesInFile(_, currParentPkgs, currPkgName))
    pkg.subPkgs.foreach(resolveSimpleReferencesInPkg(_, currParentPkgs, currPkgName))
  }

  /**
 * Resolve all references to classes and type parameters in a file
 * @param file The file to work on
 * @param parentPkgs A list of this package's parents (topmost packages at the end)
 * @param root The root package
 * @param logger The logger to use
 */
  def resolveSimpleReferencesInFile(
    file: FileNode,
    parentPkgs: List[Package],
    pkgName: String
  )(using root: RootPkg, logger: Logger): Unit = {
    val FileNode(name, pkgRef, imports, classlikes) = file

    pkgRef match {
      case Some(pkgStmt) =>
      case None =>
    }
  }
}