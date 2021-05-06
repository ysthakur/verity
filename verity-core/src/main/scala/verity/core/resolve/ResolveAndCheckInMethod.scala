package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context, Keywords}
import Context.Refs

import com.typesafe.scalalogging.Logger

//todo make a proper solver for this stuff
private def resolveStmt(
    stmt: Statement,
    mthdReturnType: Type
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): Statement | Iterable[Statement] = {
  stmt match {
    case rs: ReturnStmt => ReturnStmt(resolveExpr(rs.expr, mthdReturnType), rs.textRange)
    case lv: LocalVar =>
      val newType = lv.typ match {
        case typeRef: TypeRef => ReferenceResolve.resolveType(typeRef)
        case primitive        => primitive
      }
      lv.initExpr match {
        case Some(e) =>
          LocalVar(
              lv.modifiers,
              lv.varName,
              newType,
              Some(resolveExpr(e, newType)),
              lv.isFinal,
              lv.endInd
          )
        case None => lv
      }
    case block: Block =>
      var varRefs = ctxt.varRefs
      var context = ctxt
      val newStmts = block.stmts.map { stmt =>
        // given Context = Context(_varRefs, mthdRefs, givens, proofs, clsRefs, pkgRefs, cls, file)

        resolveStmt(stmt, mthdReturnType)(using context) match {
          case stmts: Iterable[_] =>
            Block(
                stmts.asInstanceOf[Iterable[Statement]].to(collection.mutable.ListBuffer),
                stmt.textRange
            )
          case newStmt: Statement =>
            stmt match {
              case vd: LocalVar => //Add a new variable to _varRefs
                context = Context(
                    varRefs + (vd.name -> vd),
                    context.mthdRefs,
                    context.givens,
                    context.proofs,
                    context.clsRefs,
                    context.pkgRefs,
                    context.cls,
                    context.file
                )
            }
            newStmt
        }
      }

      newStmts
  }
}

private def resolveExpr(expr: Expr, expectedType: Type)(using
    ctxt: Context,
    rootPkg: RootPkg,
    logger: Logger
): Expr = {
  val resolved = expr match {
    case MultiDotRef(path) =>
      ReferenceResolve.resolveDotChainedRef(path) match {
        case Right(resolved) =>
          resolved match {
            case e: Expr => e
            case c: ClassRef =>
              Compiler.logError(s"${c.text} is a class, not an expression", c, ctxt.file)
              expr
          }
        case Left(wrongText) =>
          Compiler.logError(
              s"Could not resolve reference ${wrongText.text}",
              wrongText.textRange,
              ctxt.file
          )
          expr
      }
    case b: BinaryExpr  => resolveBinaryExpr(b)
    case mc: MethodCall => resolveMethodCall(mc)
    case _              => ???
  }

  if (!resolved.exprType.subTypeOf(expectedType)) {
    Compiler
      .logError(s"Found ${resolved.exprType.text}, expected ${expectedType.text}", expr, ctxt.file)
  }

  resolved
}

private def resolveBinaryExpr(
    expr: BinaryExpr
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): BinaryExpr = {
  val BinaryExpr(lhs, op, rhs) = expr

  import OpType.*

  op.opType match {
    case SUBTRACT | MULTIPLY | DIVIDE =>
      val newLhs = resolveExpr(lhs, PrimitiveType.numericTypes)
      val newRhs = resolveExpr(rhs, PrimitiveType.numericTypes)

      BinaryExpr(newLhs, op, newRhs)
    case _ => ???
  }
}

private def resolveMethodCall(
    mthdCall: MethodCall
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): MethodCall = {
  val MethodCall(obj, nameText @ Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs) = mthdCall

  //Filter based on name
  val availableMthds = obj match {
    case Some(expr) =>
      val resolvedCallee = resolveExpr(expr, BuiltinTypes.objectType)
      resolvedCallee.exprType.methods.filter(_.name == mthdName)
    case None => ctxt.mthdRefs(mthdName).methods
  }

  //Filter based on number of parameters
  val possibleMthds =
    availableMthds.find(mthd =>
      mthd.params.params.size == mthdCall.valArgs.args.size
        && (mthdCall.typeArgs.isEmpty
          || mthdCall.typeArgs.get.args.size == mthd.typeParams.params.size)
        && (mthdCall.givenArgs.isEmpty
          || mthd.givenParams.isEmpty
          || mthdCall.givenArgs.get.args.size == mthd.givenParams.get.params.size)
        && (mthdCall.proofArgs.isEmpty
          || mthd.givenParams.isEmpty
          || mthdCall.proofArgs.get.args.size == mthd.proofParams.get.params.size)
    )

  //TODO Deal with overloading first!!!

  if (possibleMthds.isEmpty) {
    Compiler.logError(s"Cannot resolve method $mthdName", mthdCall)
    mthdCall
  } else if (possibleMthds.size > 1) {
    Compiler.logError(s"Ambiguous method call $mthdName", mthdCall)
    mthdCall
  } else {
    val resolvedMthd = possibleMthds.head
    val expectedTypes = resolvedMthd.params.params.map(_.paramType)
    val newValArgs =
      mthdCall.valArgs.args.lazyZip(expectedTypes).map { case (arg, expectedType) =>
        resolveExpr(arg, expectedType)
      }
    val newMthdCall =
      mthdCall.copy(valArgs = ArgList(newValArgs, ArgsKind.Normal, valArgs.textRange))
    //todo either don't return a new method or don't modify in place
    newMthdCall.resolved = Some(resolvedMthd)
    newMthdCall
  }

}
