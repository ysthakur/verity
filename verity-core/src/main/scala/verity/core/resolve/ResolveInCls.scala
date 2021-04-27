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
    pkgMap: Refs[Package],
    clsMap: Refs[Classlike],
    mthdMap: Refs[MethodGroup],
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  val fieldMap: Refs[VariableDecl] = cls.fields.view.map(f => f.name.toString -> f).toMap
  val mthdMap2 = mthdMap ++ cls.methods.view.map(m => m.name.toString -> m)
  
}
