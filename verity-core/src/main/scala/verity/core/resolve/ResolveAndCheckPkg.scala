package verity.core.resolve

import verity.ast._
import verity.ast.Pkg.Importable
import verity.ast.infile._
import verity.util._
import verity.checks.InitialPass
import verity.core.Context.Defs
import verity.core.{Compiler, Context, Keywords, CompilerMsg}

import com.typesafe.scalalogging.Logger

import scala.collection.mutable.{HashMap, ListBuffer}

/** Resolve all references to classes and type parameters in a package
  * @param pkg The package to work on
  * @param parentPkgs A list of this package's parents (topmost packages at the end)
  * @param logger The logger to use
  */
def resolveAndCheck(root: RootPkg)(using logger: Logger): Unit =
  verity.core.PackageUtil.walkWithPath(root, resolveAndCheckFile)(using root, logger)

/** Resolve all references to classes and type parameters in a file
  * @param file The file to work on
  * @param parentPkgs A non-empty list of this package's parents (topmost packages at the end)
  * @param root The root package
  * @param logger The logger to use
  */
private def resolveAndCheckFile(
    file: FileNode,
    parentPkgs: List[Pkg],
    pkgName: String
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val currPkg = parentPkgs.head
  val FileNode(name, pkgRef, imports, classlikes, jFile) = file

  val resolvedImports = file.resolvedImports

  file.classlikes.foreach(c =>
    resolveAndCheckCls(
        c,
        resolvedImports.collect { case p: Pkg => p.name -> p }.toMap,
        resolvedImports.collect { case c: Classlike => c.name -> c }.toMap,
        resolvedImports.collect { case m: MethodGroup => m.name -> m }.toMap,
        file
    ).foreach(Compiler.log(_, file))
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
    pkgDefs: Defs[Pkg],
    typeDefs: Defs[TypeDef],
    mthdRefs: Defs[MethodGroup],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Iterable[CompilerMsg] = {
  val fieldDefs: Defs[VariableDecl] = cls.fields.view.map(f => f.name -> f).toMap

  cls match {
    case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
    case _                              =>
  }

  val newMthdRefs: Defs[MethodGroup] = mthdRefs ++ cls.methodGroups.view.map(m => m.name -> m)
  logger.debug("emthodgroups" + cls.methodGroups.map(_.name).mkString(","))

  val givenDefs = cls.methods.filter(_.isGiven).toList
  val proofDefs = cls.methods.filter(_.isProof).toList

  cls.methods.flatMap { mthd =>
//    logger.debug(s"Working on method ${mthd.name}!!")
    resolveAndCheckMthd(
      mthd,
      fieldDefs,
      newMthdRefs,
      givenDefs,
      proofDefs,
      typeDefs,
      pkgDefs,
      cls,
      file
    )
  }
}

private def resolveAndCheckMthd(
    mthd: Method,
    fieldDefs: Defs[VariableDecl],
    mthdRefs: Defs[MethodGroup],
    givenDefs: Iterable[verity.core.ImplicitDef],
    proofDefs: Iterable[verity.core.ImplicitDef],
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg],
    cls: Classlike,
    file: FileNode
)(using RootPkg, Logger): List[CompilerMsg] = {
  println(s"Resolving method ${mthd.name},returntype=${mthd.returnType.text}")
  val isCtor = mthd.isInstanceOf[Constructor]

  mthd.body match {
    case Some(block) =>
      if (!mthd.isAbstract) {
        val ctxt = Context(
            fieldDefs ++ mthd.params.params.view.map(p => p.name -> p),
            mthdRefs,
            givenDefs,
            proofDefs,
            typeDefs,
            pkgDefs,
            cls,
            file
        )
        resolveStmt(block, mthd.returnType)(using ctxt).map { newBlock =>
          block.stmts.clear()
          newBlock match {
            case b: Block => block.stmts.addAll(b.stmts)
            case s => block.stmts += s
          }
        }.getOrElse(block).written
      } else {
        Nil
      }
    case _ => Nil
  }
}
