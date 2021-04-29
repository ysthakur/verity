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

//todo
/** @param cls The current class
  * @param pkgMap A map of packages that are visible outside
  * @param clsMap A map of classes that are visible outside
  * @param mthdMap A map of methods that are visible outside (does not include methods inside `cls`)
  * @param file The current file
  */
private def resolveSimpleRefsInCls(
    cls: Classlike,
    pkgRefs: Refs[Package],
    clsRefs: Refs[Classlike],
    mthdRefs: Refs[MethodGroup],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val fieldRefs: Refs[VariableDecl] = cls.fields.view.map(f => f.name.toString -> f).toMap
  val newMthdRefs = mthdRefs ++ cls.methods.view.map(m => m.name.toString -> m)
  
  cls.methods.foreach { mthd =>
    if (mthd.isCtor) {
      
    }
    resolveSimpleRefsInMthd(mthd, pkgRefs, clsRefs, newMthdRefs, cls, file)
  }
}

private def resolveSimpleRefsInMthd(
  mthd: Method,
  pkgRefs: Refs[Package],
  clsRefs: Refs[Classlike],
  mthdRefs: Refs[MethodGroup],
  cls: Classlike,
  file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {

}