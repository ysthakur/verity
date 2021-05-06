package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context, Keywords}
import verity.util.*
import verity.checks.InitialChecks
import Package.Importable
import Context.Refs

import com.typesafe.scalalogging.Logger

import scala.collection.mutable.{HashMap, ListBuffer}

/** Resolve all references to classes and type parameters in a package
  * @param pkg The package to work on
  * @param parentPkgs A list of this package's parents (topmost packages at the end)
  * @param logger The logger to use
  */
def resolveAndCheck(root: RootPkg)(using logger: Logger): Unit = {
  given RootPkg = root
  root.walkWithPath(resolveAndCheckFile)
}

/** Resolve all references to classes and type parameters in a file
  * @param file The file to work on
  * @param parentPkgs A non-empty list of this package's parents (topmost packages at the end)
  * @param root The root package
  * @param logger The logger to use
  */
private def resolveAndCheckFile(
    file: FileNode,
    parentPkgs: List[Package],
    pkgName: String
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val currPkg = parentPkgs.head
  val FileNode(name, pkgRef, imports, classlikes, jFile) = file

  InitialChecks.verifyPkgStmt(pkgRef, pkgName, name)

  val resolvedImports = file.resolvedImports

  file.classlikes.foreach(c =>
    resolveAndCheckCls(
        c,
        resolvedImports.collect { case p: Package => p.name -> p }.toMap,
        resolvedImports.collect { case c: Classlike => c.name -> c }.toMap,
        resolvedImports.collect { case m: MethodGroup => m.name -> m }.toMap,
        file
    )
  )
}

/** Perform an initial pass over this class, resolving references and finding non-type-related errors.
  *
  * @param cls The current class
  * @param pkgMap A map of packages that are visible outside
  * @param clsMap A map of classes that are visible outside
  * @param mthdMap A map of methods that are visible outside (does not include methods inside `cls`)
  * @param file The current file
  */
private[resolve] def resolveAndCheckCls(
    cls: Classlike,
    pkgRefs: Refs[Package],
    clsRefs: Refs[Classlike],
    mthdRefs: Refs[MethodGroup],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val fieldRefs: Refs[VariableDecl] = cls.fields.view.map(f => f.name -> f).toMap

  cls match {
    case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
    case _                              =>
  }

  val newMthdRefs: Refs[MethodGroup] = mthdRefs ++ cls.methodGroups.view.map(m => m.name -> m)

  val givens = cls.methods.filter(_.isGiven).toList
  val proofs = cls.methods.filter(_.isProof).toList

  cls.methods.foreach { mthd =>
    mthd match {
      case c: Constructor =>
        val mthdName = mthd.name
        if (mthdName != cls.name && mthdName != Keywords.constructorName) {
          Compiler.logError(s"Wrong constructor name: $mthdName", mthd, file)
        }
      case m: NormMethod =>
        m.returnType = m.returnType match {
          case typeRef: TypeRef =>
            ReferenceResolve.resolveType(typeRef)
          case primitive => primitive
        }
    }

    given Context = Context(fieldRefs, newMthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)

    initialPassMthd(mthd)
  }
}

private def initialPassMthd(mthd: Method)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): Unit = {
  val isCtor = mthd.isInstanceOf[Constructor]

  mthd.body match {
    case Some(block) =>
      mthd.modifiers.find(_.modType == ModifierType.ABSTRACT) match {
        case None =>
          val newBlock = (resolveStmt(block, mthd.returnType): @unchecked) match {
            case stmts: Iterable[_] => stmts.asInstanceOf[Iterable[Statement]]
          }

          block.stmts.clear()
          block.stmts.addAll(newBlock)
        case _ =>
      }
    case _ =>
  }
}
