package verity.core.resolve

import verity.ast.*
import verity.ast.Pkg.Importable
import verity.ast.infile.*
import verity.checks.InitialPass
import verity.core.Context.Defs
import verity.core.*
import verity.util.*
import cats.implicits.*
import cats.catsInstancesForId
//import com.typesafe.scalalogging.Logger

import scala.collection.mutable.{ArrayBuffer, HashMap}

type BooleanResolveResult = (Boolean, Iterable[CompilerMsg])

/** Resolve all references to classes and type parameters in a package
  * @param pkg The package to work on
  * @param parentPkgs A list of this package's parents (topmost packages at the end)
  * @param logger The logger to use
  * @return Whether or not all the files succeeded, and all their accompanying compiler messages
  */
def resolveAndCheck(root: RootPkg): Map[FileNode, BooleanResolveResult] =
  verity.core.PackageUtil.walkWithPath(root, resolveAndCheckFile)(using root).toMap

/** Resolve all references to classes and type parameters in a file
  * @param file The file to work on
  * @param parentPkgs A non-empty list of this package's parents (topmost packages at the end)
  * @param root The root package
  * @param logger The logger to use
  * @return Whether or not this file succeeded, and all the compiler messages emitted there
  */
private def resolveAndCheckFile(
  file: FileNode,
  parentPkgs: List[Pkg],
  pkgName: String
)(using rootPkg: RootPkg): BooleanResolveResult = {
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

  println(s"Classikes = ${file.classlikes}")

  reduceBooleanResolveResults(
    file.classlikes.map(c =>
      resolveAndCheckCls(
        c,
        pkgDefs,
        clsDefs,
        resolvedImports.collect { case m: MethodGroup => m.name -> m }.toMap,
        file
      )
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
  pkgDefs: Defs[Pkg],
  typeDefs: Defs[TypeDef],
  mthdRefs: Defs[MethodGroup],
  file: FileNode
): BooleanResolveResult = {
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

  val mthdLogs: BooleanResolveResult = cls.methods.view.map { mthd =>
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
  }.reduce(combineBooleanResolveResults)

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
  val fieldLogs: BooleanResolveResult =
    if (file.isSource) (false, Nil) //cls.fields.view.flatMap(resolveAndCheckField(_)(using ctxt))
    else (true, Nil)

  combineBooleanResolveResults(fieldLogs, mthdLogs)
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
): BooleanResolveResult = {
  val isCtor = mthd.isInstanceOf[Constructor]

  if (file.isSource) {
    mthd.body match {
      case Some(block) =>
        mthd.modifiers.find(_.modType == ModifierType.ABSTRACT) match {
          case Some(abstractMod) =>
            (false, List(errorMsg("Abstract methods cannot have implementations", abstractMod.textRange)))
          case None =>
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
            val resolved = resolveStmt(block, mthd.returnType, mthd.proofs)(using ctxt)
              .map { (newBlock, newProofs) =>
                block.stmts.clear()
                newBlock match {
                  case b: Block => block.stmts.addAll(b.stmts)
                  case s        => block.stmts += s
                }
              }
              .isEmpty
            (!resolved.value, resolved.written)
        }
      case _ =>
        if (mthd.isAbstract) {
          (true, Nil)
        } else {
          (
            false,
            List(
              errorMsg(
                if (mthd.isCtor) "Constructors must have implementations"
                else s"${mthd.name} must be marked abstract if it does not have a body",
                mthd.nameRange
              )
            )
          )
        }
    }
  } else (true, Nil)
}

/**
  * Combine multiple BooleanResolveResults, the same way `combineBooleanResolveResults` does.
  * If there are no results, that is taken as a success and `(true, Nil)` is returned.
  */
private[resolve] def reduceBooleanResolveResults(
  results: Iterable[BooleanResolveResult]
): BooleanResolveResult =
  try {
    if (results.isEmpty) (true, Nil)
    else results.reduce(combineBooleanResolveResults)
  } catch {
    case e: UnsupportedOperationException => throw RuntimeException(s"results=${results}, $e")
  }

/** Take two BooleanResolveResults and return another result that has failed if
  * either of them failed, and contains compiler messages from both.
  */
private[resolve] def combineBooleanResolveResults(
  result1: BooleanResolveResult,
  result2: BooleanResolveResult
): BooleanResolveResult = {
  val (success1, msgs1) = result1
  val (success2, msgs2) = result2
  (success1 && success2, msgs1 ++ msgs2)
}
