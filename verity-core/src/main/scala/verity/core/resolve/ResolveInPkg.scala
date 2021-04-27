package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context}
import verity.util.*
import verity.checks.initial.*
import Package.Importable
import Context.Refs

import com.typesafe.scalalogging.Logger

import scala.collection.mutable.HashMap

/** Resolve all references to classes and type parameters in a package
  * @param pkg The package to work on
  * @param parentPkgs A list of this package's parents (topmost packages at the end)
  * @param logger The logger to use
  */
def resolveSimpleRefs(root: RootPkg)(using logger: Logger): Unit = {
  given RootPkg = root
  root.walkWithPath(resolveSimpleRefsInFile)
}

/** Resolve all references to classes and type parameters in a file
  * @param file The file to work on
  * @param parentPkgs A non-empty list of this package's parents (topmost packages at the end)
  * @param root The root package
  * @param logger The logger to use
  */
private def resolveSimpleRefsInFile(
    file: FileNode,
    parentPkgs: List[Package],
    pkgName: String
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val currPkg = parentPkgs.head
  val FileNode(name, pkgRef, imports, classlikes, jFile) = file

  InitialChecks.verifyPkgStmt(pkgRef, pkgName, name)

  val imptsMap = resolveImports(imports, file)

  val pkgMap = HashMap[String, Package]()
  val clsMap = HashMap[String, Classlike]()
  val mthdMap = HashMap[String, MethodGroup]()

  //todo find a way to reduce code duplication
  pkgMap.addAll(rootPkg.subPkgs.view.map(p => p.name -> p))
  clsMap.addAll(currPkg.classlikes.map(c => c.name.toString -> c))

  imptsMap.foreach { case (name, imported, imptStmt) =>
    imported match {
      case pkg: Package =>
        if (pkgMap.contains(name)) {
          Compiler.logError(
              s"Cannot import package ${name}: package of same name already in scope",
              imptStmt,
              file
          )
        } else {
          pkgMap += name -> pkg
        }
      case cls: Classlike =>
        if (clsMap.contains(name)) {
          Compiler.logError(
              s"Cannot import class ${cls.name}: class of same name already in scope",
              imptStmt,
              file
          )
        } else {
          clsMap += name -> cls
        }
      case mthd: MethodGroup =>
        if (mthdMap.contains(name)) {
          mthdMap(name) = mthdMap(name).merge(mthd)
        } else {
          mthdMap += name -> mthd
        }
    }
  }

  val pkgIMap = pkgMap.toMap
  val clsIMap = clsMap.toMap
  val mthdIMap = mthdMap.toMap

  file.classlikes.foreach(c => resolveSimpleRefsInCls(c, pkgIMap, clsIMap, mthdIMap, file))
}

private def resolveImports(
    imports: Iterable[ImportStmt],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Iterable[(String, Importable, ImportStmt)] =
  imports.view.flatMap { case imptStmt @ ImportStmt(DotRef(dotRef), _, wildcard) =>
    val path = dotRef.view.map(_.text)
    Package
      .findImptableAbs(path)
      .fold {
        logger.error(s"Not found $dotRef")
        Nil
      } { impt =>
        if (!wildcard) {
          Seq((path.last, impt, imptStmt))
        } else {
          impt match {
            case pkg: Package =>
              pkg.subPkgs.view.map(p => (p.name, p, imptStmt))
                ++ pkg.classlikes.map(c => (c.name.toString, c, imptStmt))
            case cls: Classlike =>
              cls.children.view.map(c => (c.name.toString, c, imptStmt))
            case _ =>
              Compiler.logError(s"Cannot import members of ${impt.name}", imptStmt, file)
              Nil
          }
        }
      }

  }
