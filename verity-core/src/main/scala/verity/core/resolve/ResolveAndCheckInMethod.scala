package verity.core.resolve

import com.typesafe.scalalogging.Logger
import verity.ast.*
import verity.ast.infile.*
import verity.core.Context.Defs
import verity.core.{Compiler, Context, Keywords}

//todo make a proper solver for this stuff
private def resolveStmt(
    stmt: Statement,
    mthdReturnType: Type
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): Statement | Iterable[Statement] = {
  logger.debug(s"Resolving statement ${stmt.text}, ${stmt.getClass}")
  stmt match {
    case rs: ReturnStmt => ReturnStmt(resolveAndCheckExpr(rs.expr, mthdReturnType), rs.textRange)
    case lv: LocalVar =>
      logger.debug(s"Resolving localvar ${lv.text}")
      val newType = lv.typ match {
        case typeRef: TypeRef =>
          logger.debug(s"Localvar has typeref ${typeRef.text}")
          ReferenceResolve.resolveType(typeRef)
        case primitive        =>
          logger.debug(s"Localvar has primitive type ${primitive.text}")
          primitive
      }
      lv.initExpr match {
        case Some(e) =>
          logger.debug("Resolving initExpr ${e.text} for local var")
          LocalVar(
              lv.modifiers,
              lv.varName,
              newType,
              Some(resolveAndCheckExpr(e, newType)),
              lv.endInd
          )
        case None =>
          logger.debug("No initExpr for local var")
          lv
      }
    case block: Block =>
      //A variable because new variable declarations will be added as it goes down the block
      var context = ctxt
      val newStmts = block.stmts.map { stmt =>
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
                    context.varDefs + (vd.name -> vd),
                    context.mthdDefs,
                    context.givenDefs,
                    context.givenDefs,
                    context.typeDefs,
                    context.pkgDefs,
                    context.cls,
                    context.file
                )
            }
            newStmt
        }
      }

      newStmts
    case _ => logger.debug("Did not resolve statement!"); ???
  }
}

private def resolveAndCheckExpr(expr: Expr, expectedType: Type)(using
    ctxt: Context,
    logger: Logger
): Expr = {
  logger.debug(s"Resolving expr ${expr.text}")

  import Unresolved.*

  val resolved: Expr = expr match {
    case UnresolvedFieldAccess(obj, fieldName) =>
      val owner = resolveAndCheckExpr(obj, BuiltinTypes.objectType)
      ReferenceResolve
        .findField(owner.exprType, fieldName.text)
        .fold(expr)(field => FieldAccess(owner, field, fieldName.textRange))
    case mc: UnresolvedMethodCall =>
      (mc.objOrCls: @unchecked) match {
        case None => resolveUnresolvedMethodCall(ClassRef(ctxt.cls, None, TextRange.synthetic), mc)
        case Some(e: Expr) => resolveUnresolvedMethodCall(resolveAndCheckExpr(e, BuiltinTypes.objectType), mc)
        case Some(MultiDotRef(path)) =>
          ReferenceResolve.resolveDotChainedRef(path) match {
            case Right(caller) => resolveUnresolvedMethodCall(caller, mc)
            case Left(errorMsg, textRange) =>
              Compiler.logError(errorMsg, textRange)
              expr
          }
      }
    case b: BinaryExpr  => resolveBinaryExpr(b)
    case mc: MethodCall => resolveMethodCall(mc)
    case _              => logger.error(s"aahh!! ${expr}");???
  }

  if (!resolved.exprType.subTypeOf(expectedType)) {
    Compiler
      .logError(s"Found ${resolved.exprType.text}, expected ${expectedType.text}", expr, ctxt.file)
  }

  resolved
}

private def resolveUnresolvedMethodCall(caller: Expr | ClassRef, mthdCall: Unresolved.UnresolvedMethodCall): MethodCall = {
  ???
}

private def resolveBinaryExpr(
    expr: BinaryExpr
)(using ctxt: Context, logger: Logger): BinaryExpr = {
  val BinaryExpr(lhs, op, rhs) = expr
  import OpType.*
  op.opType match {
    case SUBTRACT | MULTIPLY | DIVIDE =>
      val newLhs = resolveAndCheckExpr(lhs, PrimitiveType.numericTypes)
      val newRhs = resolveAndCheckExpr(rhs, PrimitiveType.numericTypes)

      BinaryExpr(newLhs, op, newRhs)
    case _ => ???
  }
}

private def resolveMethodCall(
    mthdCall: MethodCall
)(using ctxt: Context, logger: Logger): MethodCall = {
  val MethodCall(obj, nameText @ Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs) = mthdCall

  //Filter based on name
  val availableMthds = obj match {
    case Some(expr) =>
      val resolvedCallee = resolveAndCheckExpr(expr, BuiltinTypes.objectType)
      resolvedCallee.exprType.methods.filter(_.name == mthdName)
    case None => ctxt.mthdDefs(mthdName).methods
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
    val expectedTypes = resolvedMthd.params.params.map(_.typ)
    val newValArgs =
      mthdCall.valArgs.args.lazyZip(expectedTypes).map { case (arg, expectedType) =>
        resolveAndCheckExpr(arg, expectedType)
      }
    val newMthdCall =
      mthdCall.copy(valArgs = ArgList(newValArgs, ArgsKind.Normal, valArgs.textRange))
    //todo either don't return a new method or don't modify in place
    newMthdCall.resolved = Some(resolvedMthd)
    newMthdCall
  }

}
