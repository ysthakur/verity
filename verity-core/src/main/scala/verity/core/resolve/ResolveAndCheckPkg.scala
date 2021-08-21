package verity.core.resolve

import verity.ast._
import verity.ast.Pkg.Importable
import verity.ast.infile._
import verity.checks.InitialPass
import verity.core.Context.Defs
import verity.core._
import verity.util._
import cats.implicits._
import cats.catsInstancesForId
//import com.typesafe.scalalogging.Logger

import scala.collection.mutable.{ArrayBuffer, HashMap}

/** Resolve all references to classes and type parameters in a package
  * @param pkg The package to work on
  * @param parentPkgs A list of this package's parents (topmost packages at the end)
  * @param logger The logger to use
  */
def resolveAndCheck(root: RootPkg): Unit =
  verity.core.PackageUtil.walkWithPath(root, resolveAndCheckFile)(using root)

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
)(using rootPkg: RootPkg): Unit = {
  val currPkg = parentPkgs.head
  val FileNode(name, pkgRef, imports, classlikes, jFile, _) = file

  val resolvedImports = file.resolvedImports

  val clsDefs =
    (file.classlikes ++ resolvedImports.collect { case c: Classlike => c }).view
      .map(c => c.name -> c)
      .toMap
  val pkgDefs =
    (resolvedImports.collect { case p: Pkg => p }.view ++ rootPkg.subPkgs)
      .map(p => p.name -> p)
      .toMap

  file.classlikes.foreach(c =>
    resolveAndCheckCls(
      c,
      pkgDefs,
      clsDefs,
      resolvedImports.collect { case m: MethodGroup => m.name -> m }.toMap,
      file
    ).foreach(LogUtils.log(_, file))
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
): Iterable[CompilerMsg] = {
  val fieldDefs: Defs[VariableDecl] = cls.fields.view.map(f => f.name -> f).toMap

  if (file.isSource) {
    cls match {
      case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
      case _                              =>
    }
  }

  val newMthdRefs: Defs[MethodGroup] = mthdRefs ++ cls.methodGroups.view.map(m => m.name -> m)
//  logger.debug("emthodgroups" + cls.methodGroups.map(_.name).mkString(","))

  val givenDefs = cls.methods.filter(_.isGiven).toList
  val proofDefs = cls.methods.filter(_.isProof).toList

  val mthdLogs = cls.methods.flatMap { mthd =>
//    logger.debug(s"Working on method ${mthd.name}!!")
    resolve.resolveAndCheckMthd(
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

  val ctxt = Context(
    fieldDefs,
    newMthdRefs,
    givenDefs,
    proofDefs,
    typeDefs,
    pkgDefs,
    cls,
    file
  )
  val fieldLogs: Iterable[CompilerMsg] =
    if (file.isSource) cls.fields.view.flatMap(resolveAndCheckField(_)(using ctxt))
    else Nil

  fieldLogs ++ mthdLogs
}

private def resolveAndCheckField(field: Field)(using Context): List[CompilerMsg] = {
  // val resolvedTyp = ReferenceResolve.resolveTypeIfNeeded(field.typ)(using ctxt)
  // resolvedTyp.map { t => field.typ = t }

  field.initExpr.fold(Nil) { e =>
    val resolved: ResultWithLogs[Option[Expr]] =
      resolveAndCheckExpr(e, field.typ).map(_._1).value
    resolved.map {
      case None =>
      case s    => field.initExpr = s
    }
    resolved.written
  }
}

private def resolveAndCheckMthd(
  mthd: Method,
  fieldDefs: Defs[VariableDecl],
  mthdRefs: Defs[MethodGroup],
  givenDefs: List[GivenDef],
  proofDefs: List[GivenDef],
  typeDefs: Defs[TypeDef],
  pkgDefs: Defs[Pkg],
  cls: Classlike,
  file: FileNode
): List[CompilerMsg] = {
  val isCtor = mthd.isInstanceOf[Constructor]

  if (file.isSource) {
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
          resolveStmt(block, mthd.returnType, mthd.proofs)(using ctxt)
            .map { (newBlock, newProofs) =>
              block.stmts.clear()
              newBlock match {
                case b: Block => block.stmts.addAll(b.stmts)
                case s        => block.stmts += s
              }
            }
            .getOrElse(block)
            .written
        } else {
          Nil
        }
      case _ => Nil
    }
  } else Nil
}
