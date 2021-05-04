package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context, Keywords}
import Context.Refs

import com.typesafe.scalalogging.Logger

private def initialPassStmt(
    stmt: Stmt,
    varRefs: Refs[VariableDecl],
    mthdRefs: Refs[MethodGroup],
    givens: List[Expr | Methodlike],
    proofs: List[Expr | Methodlike],
    clsRefs: Refs[Classlike],
    pkgRefs: Refs[Package],
    mthdReturnType: Type,
    cls: Classlike,
    file: FileNode
)(using rootPkg: RootPkg, logger: Logger): Unit = {
  stmt match {
    case rs: ReturnStmt =>
      initialPassExpr(rs.expr, varRefs, mthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)
      if (!rs.expr.exprType.subTypeOf(mthdReturnType)) {
          Compiler.logError("Wrong return type", rs.expr, file)
      }
    case block: Block =>
      var _varRefs = varRefs
      given Context = Context(varRefs, mthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)
      for (stmt <- block.stmts) {
        // given Context = Context(_varRefs, mthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)

        initialPassStmt(stmt, pkgRefs, clsRefs, mthdRefs, _varRefs, cls, file)

        stmt match {
          case vd: VariableDecl => //todo add a new variable to _varRefs
        }
      }
  }
}

private def initialPassExpr(
    expr: Expr,
    _varRefs: Refs[VariableDecl],
    mthdRefs: Refs[MethodGroup],
    givens: List[Expr | Methodlike],
    proofs: List[Expr | Methodlike],
    clsRefs: Refs[Classlike],
    pkgRefs: Refs[Package],
    expectedType: Type,
    cls: Classlike,
    file: FileNode
)(using /*ctxt: Context, */rootPkg: RootPkg, logger: Logger): Unit = {}
