package verity.core.resolve

import com.typesafe.scalalogging.Logger
import verity.ast.*
import verity.ast.infile.{ResolvedOrUnresolvedExpr, unresolved => ur, *}
import verity.core.Context.Defs
import verity.core.{Compiler, Context, ErrorMsg, Keywords, ResolveResult}

//todo make a proper solver for this stuff
private def resolveStmt(
    stmt: Statement,
    mthdReturnType: Type
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): Statement | Iterable[Statement] = {
//  logger.debug(s"Resolving statement ${stmt.text}, ${stmt.getClass}")
  stmt match {
    case rs: ReturnStmt =>
      resolveAndCheckExpr(rs.expr, mthdReturnType)
        .map { expr => ReturnStmt(expr, rs.textRange) }
        .left.map(Compiler.logError)
        .getOrElse(rs)
    case lv: LocalVar =>
//      logger.debug(s"Resolving localvar ${lv.text}")
      val newType = ReferenceResolve.resolveTypeIfNeeded(lv.typ)
      lv.initExpr match {
        case Some(e) =>
//          logger.debug(s"Resolving initExpr ${e.text} for local var")
          LocalVar(
              lv.modifiers,
              lv.varName,
              newType,
              Some(resolveAndCheckExpr(e, newType).getOrElse(e)),
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
                stmt.textRange,
                ???
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

private def resolveAndCheckExpr(expr: ResolvedOrUnresolvedExpr, expectedType: Type)(using
    ctxt: Context,
    logger: Logger
): ResolveResult[Expr] = {
//  logger.debug(s"Resolving expr ${expr.text}")

  val resolved: ResolveResult[Expr] = expr match {
    case ur.UnresolvedFieldAccess(obj, fieldName) =>
      resolveAndCheckExpr(obj, BuiltinTypes.objectType).flatMap { owner =>
          ReferenceResolve
            .findField(owner.typ, fieldName.text) match {
              case Some(field) => Right(FieldAccess(owner, field, fieldName.textRange))
              case None => Left(ErrorMsg(s"No such field ${fieldName.text}", fieldName))
            }
      }
    case mc: ur.UnresolvedMethodCall => resolveUnresolvedMethodCall(mc)
    case ur.MultiDotRefExpr(path) =>
      ReferenceResolve.resolveDotChainedRef(path).flatMap {
        case e: Expr => Right(e)
        case c => Left(ErrorMsg(s"${c.text} is a class, not an expression", c))
      }
    case b: ur.UnresolvedBinaryExpr => resolveBinaryExpr(b)
//    case mc: MethodCall => resolveMethodCall(mc)
    case _              => Left(ErrorMsg(s"aahh!! $expr wrong type!!! ${expr.getClass}", expr))
  }
//  logger.debug(s"resolved ${resolved.map(_.text)}, expected: ${expectedType.text}")

  resolved.flatMap { expr =>
    if (!expr.typ.subTypeOf(expectedType)) {
      Left(ErrorMsg(s"${expr.text} is of wrong type: found ${expr.typ.text}, expected ${expectedType.text}", expr))
    } else {
      logger.debug(s"Type matched for ${expr.text}: ${expr.typ.text}!")
      Right(expr)
    }
  }.left.map{e => Compiler.logError(e); e}
}

private def resolveUnresolvedMethodCall(
    mthdCall: ur.UnresolvedMethodCall
)(using ctxt: Context, logger: Logger): Either[ErrorMsg, MethodCall] = {
  val ur.UnresolvedMethodCall(_, nameText@Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs) = mthdCall

  def resolveHelper(caller: Option[Expr | ClassRef]): ResolveResult[MethodCall] = {
    //Filter based on name
    val availableMthds = caller match {
      case Some(exprOrCls) =>
        val allMethods = exprOrCls match {
          case e: Expr => resolveAndCheckExpr(e, BuiltinTypes.objectType).map(_.typ.methods).getOrElse(Nil)
          case c: ClassRef => c.cls.methods
        }
        allMethods.filter(_.name == mthdName)
      case None => ctxt.mthdDefs(mthdName).methods
    }

    //Filter based on number of parameters
    val possibleMthds =
      availableMthds.find(mthd =>
        mthd.params.params.size == mthdCall.valArgs.args.size
          && (mthdCall.typeArgs.isEmpty
          || mthdCall.typeArgs.args.size == mthd.typeParams.params.size)
          && (mthdCall.givenArgs.isEmpty
          || mthd.givenParams.isEmpty
          || mthdCall.givenArgs.get.args.size == mthd.givenParams.get.params.size)
          && (mthdCall.proofArgs.isEmpty
          || mthd.givenParams.isEmpty
          || mthdCall.proofArgs.get.args.size == mthd.proofParams.get.params.size)
      )

    //TODO Deal with overloading first!!!

    if (possibleMthds.isEmpty) {
      Left(ErrorMsg(s"Cannot resolve method $mthdName", mthdCall))

    } else if (possibleMthds.size > 1) {
      Left(ErrorMsg(s"Ambiguous method call $mthdName", mthdCall))
    } else {
      val resolvedMthd = possibleMthds.head
      val expectedTypes = resolvedMthd.params.params.map(_.typ)
      val (failedArgs, succeededArgs) =
        mthdCall.valArgs.args.lazyZip(expectedTypes).map { case (arg, expectedType) =>
          resolveAndCheckExpr(arg, expectedType)
        }.partition(_.isLeft)
      //todo log failedArgs
      val newMthdCall =
        MethodCall(
          caller,
          nameText,
          ArgList(succeededArgs.map(_.toOption.get), ArgsKind.Normal, valArgs.textRange),
          typeArgs,
          ???,//givenArgs,
          ???,//proofArgs,
          resolvedMthd.returnType
        )
      //todo either don't return a new method or don't modify in place
      newMthdCall.resolved = Some(resolvedMthd)

      Right(newMthdCall)
    }
  }

  (mthdCall.objOrCls: @unchecked) match {
    case None => resolveHelper(None)
    case Some(e: Expr) => resolveHelper(resolveAndCheckExpr(e, BuiltinTypes.objectType).toOption)
    case Some(ur.MultiDotRef(path)) =>
      ReferenceResolve.resolveDotChainedRef(path) match {
        case Right(caller) => resolveHelper(Some(caller))
        case Left(error) => Left(error)
      }
  }
}

private def resolveBinaryExpr(
    expr: ur.UnresolvedBinaryExpr
)(using ctxt: Context, logger: Logger): ResolveResult[BinaryExpr] = {
  val ur.UnresolvedBinaryExpr(lhs, op, rhs) = expr
  import OpType.*
  op.opType match {
    case SUBTRACT | MULTIPLY | DIVIDE =>
      for {
        newLhs <- resolveAndCheckExpr(lhs, PrimitiveTypeDef.numericTypes)
        newRhs <- resolveAndCheckExpr(rhs, PrimitiveTypeDef.numericTypes)
      } yield BinaryExpr(newLhs, op, newRhs, ???)
    case _ => Left(ErrorMsg("Operator not implemented", op))
  }
}
/*
private def resolveMethodCall(
    mthdCall: MethodCall
)(using ctxt: Context, logger: Logger): MethodCall = {
  val MethodCall(obj, nameText @ Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs, typ) = mthdCall

  //Filter based on name
  val availableMthds = obj match {
    case Some(exprOrCls) =>
      val allMethods = exprOrCls match {
        case e: Expr => resolveAndCheckExpr(e, BuiltinTypes.objectType).map(_.typ.methods).getOrElse(Nil)
        case c: ClassRef => c.cls.methods
      }
      allMethods.filter(_.name == mthdName)
    case None => ctxt.mthdDefs(mthdName).methods
  }

  //Filter based on number of parameters
  val possibleMthds =
    availableMthds.find(mthd =>
      mthd.params.params.size == mthdCall.valArgs.args.size
        && (mthdCall.typeArgs.isEmpty
          || mthdCall.typeArgs.args.size == mthd.typeParams.params.size)
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
    val (failedArgs, succeededArgs) =
      mthdCall.valArgs.args.lazyZip(expectedTypes).map { case (arg, expectedType) =>
        resolveAndCheckExpr(arg, expectedType)
      }.partition(_.isLeft)
    //todo log failedArgs
    val newMthdCall =
      mthdCall.copy(valArgs = ArgList(succeededArgs.map(_.toOption.get), ArgsKind.Normal, valArgs.textRange))
    //todo either don't return a new method or don't modify in place
    newMthdCall.resolved = Some(resolvedMthd)
    newMthdCall
  }

}
*/