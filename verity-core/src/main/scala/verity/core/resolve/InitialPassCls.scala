package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Keywords}
import verity.core.Context.Refs
import verity.util.*
import verity.checks.initial.*
import Package.Importable

import com.typesafe.scalalogging.Logger

// import scala.collection.mutable.HashMap

//todo
/** Perform an initial pass over this class, resolving references and finding non-type-related errors.
  *
  * @param cls The current class
  * @param pkgMap A map of packages that are visible outside
  * @param clsMap A map of classes that are visible outside
  * @param mthdMap A map of methods that are visible outside (does not include methods inside `cls`)
  * @param file The current file
  */
private[resolve] def initialPassCls(
    cls: Classlike,
    pkgRefs: Refs[Package],
    clsRefs: Refs[Classlike],
    mthdRefs: Refs[MethodGroup],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val fieldRefs: Refs[VariableDecl] = cls.fields.view.map(f => f.name -> f).toMap

  cls match {
    case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
    case _ =>
  }

  val newMthdRefs: Refs[MethodGroup] = mthdRefs ++ cls.methodGroups.view.map(m => m.name -> m)

  val givens = cls.methods.filter(_.isGiven)
  val proofs = cls.methods.filter(_.isProof)

  cls.methods.foreach { mthd =>
    mthd match {
      case c: Constructor =>
        val mthdName = mthd.name
        if (mthdName != cls.name && mthdName != Keywords.constructorName) {
          Compiler.logError(s"Wrong constructor name: $mthdName", mthd, file)
        }
      case m: NormMethod =>
        m.returnType = ReferenceResolve.resolveType(m.returnType, clsRefs, pkgRefs)
    }
    initialPassMthd(mthd, fieldRefs, newMthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)
  }
}

private def initialPassMthd(
    mthd: Method,
    fieldRefs: Refs[VariableDecl],
    mthdRefs: Refs[MethodGroup],
    givens: List[Expr | Methodlike],
    proofs: List[Expr | Methodlike],
    clsRefs: Refs[Classlike],
    pkgRefs: Refs[Package],
    cls: Classlike,
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val isCtor = mthd.isInstanceOf[Constructor]

  mthd.body match {
    case Some(block) =>
      mthd.modifiers.find(_.modType == ModifierType.ABSTRACT) match {
        case Some(mod) =>
          Compiler.logError("Method with implementation cannot be abstract", mod, file)
        case None =>
          //actual checking should occur here
          //todo check if constructor's first statement is super or this
          initialPassStmt(block, fieldRefs, mthdRefs, givens, proofs, clsRefs, pkgRefs, mthd.returnType, cls, file)
      }
    case None =>
      if (!mthd.isAbstract)
        Compiler.logError("Method requires abstract modifier or implementation", mthd, file)
  }
}
