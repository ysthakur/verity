package verity.core.resolve

import verity.ast._
import verity.ast.infile.{unresolved => ur, _}
import verity.core._
import verity.core.Context.Defs
import verity.ast.infile.unresolved.UnresolvedArgList
import com.typesafe.scalalogging.Logger

import cats.data.{Writer, OptionT}
import cats.implicits._

import scala.collection.mutable

//todo make a proper solver for this stuff
private def resolveStmt(
    stmt: Statement,
    mthdReturnType: Type
)(using ctxt: Context, rootPkg: RootPkg, logger: Logger): ResolveResult[Statement] = {
//  logger.debug(s"Resolving statement ${stmt.text}, ${stmt.getClass}")
  stmt match {
    case ues: ur.UnresolvedExprStmt =>
      resolveAndCheckExpr(ues.expr, UnknownType).map(e => new ExprStmt(e, ues.end))
    case rs: ReturnStmt =>
      resolveAndCheckExpr(rs.expr, mthdReturnType).map { expr => ReturnStmt(expr, rs.textRange) }
    case lv: LocalVar =>
//      logger.debug(s"Resolving localvar ${lv.text}")
      ReferenceResolve.resolveTypeIfNeeded(lv.typ).flatMap { newType =>
        lv.initExpr match {
          case Some(origExpr) =>
            resolveAndCheckExpr(origExpr, newType).map { newExpr =>
              LocalVar(lv.modifiers, lv.varName, newType, Some(newExpr), lv.endInd)
            }
          case None =>
            logger.debug("No initExpr for local var")
            OptionT.some(LocalVar(
              lv.modifiers,
              lv.varName,
              newType,
              None,
              lv.endInd
            ))
        }
      }
    case block: Block =>
      //Traverse the block, keeping track of the a Context holding new variable declarations
      //and a ListBuffer containing resolved statements
      val ctxtAndNewStmts: ResolveResult[(Context, mutable.ListBuffer[Statement])] =
        block.stmts.foldLeft(
          OptionT.some(ctxt -> mutable.ListBuffer()): ResolveResult[(Context, mutable.ListBuffer[Statement])]
        ) { case (acc, stmt) =>
          acc.flatMap { case context -> prev =>
            resolveStmt(stmt, mthdReturnType)(using context)
              .orElse(OptionT.some(stmt)) //If the statement could not be resolved, keep the old one
              .map { newStmt =>
                val newCtxt = stmt match {
                  case vd: LocalVar => //Add a new variable to _varRefs
                    Context(
                      context.varDefs + (vd.name -> vd),
                      context.mthdDefs,
                      context.givenDefs,
                      context.proofDefs,
                      context.typeDefs,
                      context.pkgDefs,
                      context.cls,
                      context.file
                    )
                  case _ => context
                }
                newCtxt -> (prev += newStmt)
              }
          }
      }

      ctxtAndNewStmts.map((_, stmts) =>
        Block(stmts, block.textRange, if (stmts.isEmpty) VoidTypeRef(TextRange.synthetic) else statementType(stmts.last))
      )
    case x => println("Did not resolve statement!"); OptionT.none
  }
}

private def resolveAndCheckExpr(
    expr: ResolvedOrUnresolvedExpr,
    expectedType: Type)(using ctxt: Context): ResolveResult[Expr] = {
//  logger.debug(s"Resolving expr ${expr.text}")

  val resolved: ResolveResult[Expr] = expr match {
    case ur.UnresolvedFieldAccess(obj, fieldName) =>
      resolveAndCheckExpr(obj, BuiltinTypes.objectType).flatMap { owner =>
          ReferenceResolve
            .findField(owner.typ, fieldName.text) match {
              case Some(field) => OptionT.some(FieldAccess(owner, field, fieldName.textRange))
              case None => singleMsg(errorMsg(s"No such field ${fieldName.text}", fieldName))
            }
      }
    case mc: ur.UnresolvedMethodCall => resolveUnresolvedMethodCall(mc)
    case ur.MultiDotRefExpr(path) =>
      ReferenceResolve.resolveDotChainedRef(path).flatMap {
        case e: Expr => OptionT.some(e)
        case c => singleMsg(errorMsg(s"${c.text} is a class, not an expression", c))
      }
    case b: ur.UnresolvedBinaryExpr => resolveBinaryExpr(b)
    case upe: ur.UnresolvedParenExpr => resolveAndCheckExpr(upe.expr, expectedType).map(ParenExpr(_, upe.textRange))
//    case mc: MethodCall => resolveMethodCall(mc)
    case _              => singleMsg(errorMsg(s"aahh!! $expr wrong type!!! ${expr.getClass}", expr))
  }
//  logger.debug(s"resolved ${resolved.map(_.text)}, expected: ${expectedType.text}")

  resolved.flatMap { expr =>
    if (!expr.typ.subTypeOf(expectedType)) {
      singleMsg(
        errorMsg(
          s"${expr.text} is of wrong type: found ${expr.typ.text}, expected ${expectedType.text}",
          expr
        )
      )
    } else {
      println(s"Type matched for ${expr.text}: ${expr.typ.text}!")
      OptionT.some(expr)
    }
  }
}

//TODO Deal with overloading first!!!
private def resolveUnresolvedMethodCall(
    mthdCall: ur.UnresolvedMethodCall
)(using ctxt: Context): ResolveResult[Expr] = {
  val ur.UnresolvedMethodCall(_, nameText@Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs) = mthdCall

  def resolveHelper(caller: Option[Expr | ClassRef]): ResolveResult[Expr] = {
    (caller match {
        case None => OptionT(Writer(Nil, Some(ctxt.mthdDefs(mthdName).methods)))
        case Some(e: Expr) => resolveAndCheckExpr(e, BuiltinTypes.objectType).map(_.typ.methods)
        case Some(c: ClassRef) => OptionT(Writer(Nil, Some(c.cls.methods)))
    }: ResolveResult[Iterable[Method]]).flatMap { allMthds =>
      //Filter based on name and number of parameters
      val possibleMthds = allMthds.filter(mthd =>
        mthd.name == mthdName
          && mthd.params.params.size == mthdCall.valArgs.args.size
          && (mthdCall.typeArgs.isEmpty
            || mthdCall.typeArgs.args.size == mthd.typeParams.params.size)
          && (mthdCall.givenArgs.isEmpty
            || mthd.givenParams.isEmpty
            || mthdCall.givenArgs.get.args.size == mthd.givenParams.get.params.size)
          && (mthdCall.proofArgs.isEmpty
            || mthd.givenParams.isEmpty
            || mthdCall.proofArgs.get.args.size == mthd.proofParams.get.params.size)
      )

      resolveUnresolvedMethodCall(mthdCall, caller, possibleMthds)
    }
  }

  (mthdCall.objOrCls: @unchecked) match {
    case None => resolveHelper(None)
    case Some(e: Expr) => resolveAndCheckExpr(e, BuiltinTypes.objectType).flatMap(c => resolveHelper(Some(c)))
    case Some(ur.MultiDotRef(path)) =>
      ReferenceResolve.resolveDotChainedRef(path).flatMap { caller => resolveHelper(Some(caller)) }
  }
}

private def resolveUnresolvedMethodCall(
    mthdCall: ur.UnresolvedMethodCall,
    caller: Option[Expr | ClassRef],
    possibleMthds: Iterable[Method]
)(using ctxt: Context): ResolveResult[Expr] = {
  println(s"resolving method call ${mthdCall.text}, ${possibleMthds.map(_.name)}")
  val ur.UnresolvedMethodCall(_, nameText@Text(mthdName), valArgs, typeArgs, givenArgs, proofArgs) = mthdCall

  if (possibleMthds.isEmpty) {
    singleMsg(errorMsg(s"Cannot resolve method $mthdName", mthdCall))
  } else if (possibleMthds.size > 1) {
    singleMsg(errorMsg(s"Ambiguous method call $mthdName", mthdCall))
  } else {
    val resolvedMthd = possibleMthds.head
    for {
      newValArgs <- resolveArgList(mthdCall.valArgs, resolvedMthd.params.params.map(_.typ))
      newGivenArgs <- resolveImplicitArgList(mthdCall.givenArgs, resolvedMthd.givenParams)
      newProofArgs <- resolveImplicitArgList(mthdCall.proofArgs, resolvedMthd.proofParams)
    } yield MethodCall(
      caller,
      nameText,
      newValArgs,
      typeArgs,
      newGivenArgs,
      newProofArgs,
      resolvedMthd.returnType,
      resolvedMthd
    )
  }
}

private def resolveImplicitArgList(
    argList: Option[UnresolvedArgList],
    params: Option[ParamList]
)(using ctxt: Context): ResolveResult[Option[ArgList]] = {
  argList match {
    case Some(origArgs) =>
      params match {
        case Some(paramList) => resolveArgList(origArgs, paramList.params.map(_.typ)).map(Some.apply)
        case None => singleMsg(errorMsg(s"No ${origArgs.argsKind} arguments expected", origArgs))
      }
    case None =>
      params match {
        case Some(paramList) => ??? //TODO resolve implicit arguments
        case None => OptionT(Writer(Nil, Some(None)))
      }
  }
}

private def resolveArgList(
    origArgs: UnresolvedArgList,
    expectedTypes: Iterable[Type]
)(using ctxt: Context): ResolveResult[ArgList] = {
//  println(s"resolving arglist ${origArgs.text}, ${expectedTypes.map(_.text)}")
  origArgs.args
    .lazyZip(expectedTypes)
    .map(resolveAndCheckExpr)
    .foldLeft(OptionT.some(Nil): ResolveResult[List[Expr]])((resolvedArgs, currArg) =>
      for {
        args <- resolvedArgs
        arg <- currArg
      } yield arg :: args
    ).map(resolvedArgs => ArgList(resolvedArgs.reverse, origArgs.argsKind, origArgs.textRange))
}


private def resolveBinaryExpr(
    expr: ur.UnresolvedBinaryExpr
)(using ctxt: Context): ResolveResult[Expr] = {
  val ur.UnresolvedBinaryExpr(lhs, op, rhs) = expr
  import OpType._
  op.opType match {
    case SUBTRACT | MULTIPLY | DIVIDE =>
      for {
        newLhs <- resolveAndCheckExpr(lhs, PrimitiveTypeDef.numericTypes)
        newRhs <- resolveAndCheckExpr(rhs, PrimitiveTypeDef.numericTypes)
      } yield BinaryExpr(newLhs, op, newRhs, ???)
    case _ => OptionT(Writer(errorMsg("Operator not implemented", op) :: Nil, None))
  }
}

private def statementType(stmt: Statement): Type = stmt match {
  case ht: HasType => ht.typ
  case _ => VoidTypeRef(TextRange.synthetic)
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