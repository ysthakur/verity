// package verity.compiler.core.resolve

// import verity.compiler.ast._
// import verity.compiler.ast.infile.{unresolved => ur, _}
// import verity.compiler.core._
// import verity.compiler.core.Context.Defs

// //import cats._
// //import cats.catsInstancesForId
// //import cats.data.{OptionT, Writer}
// //import cats.implicits._
// //import com.typesafe.scalalogging.Logger

// import scala.collection.mutable



// //todo make a proper solver for this stuff
// private def resolveStmt(
//   stmt: Statement,
//   mthdReturnType: Type,
//   mthdProofs: Iterable[Type]
// )(using msgs: Messages, ctxt: Context): Option[(Statement, List[ProofDef])] = {
// //  logger.debug(s"Resolving statement ${stmt.text}, ${stmt.getClass}")
//   stmt match {
//     case ues: ExprStmt =>
//       resolveAndCheckExpr(ues.expr, UnknownType).map { (expr, proofs) =>
//         (new ExprStmt(expr), proofs)
//       }
//     case rs: ReturnStmt =>
//       for {
//         newReturnStmt <- rs.expr match {
//           case None => OptionT(Writer(Nil, Some(rs)))
//           case Some(oldExpr) =>
//             resolveAndCheckExpr(oldExpr, mthdReturnType).map { (newExpr, proofs) =>
//               ReturnStmt(Some(newExpr), rs.textRange)
//             }
//         }: Option[ReturnStmt]
//         _ <- mthdProofs.toList
//           .map(ImplicitSearch.findProof(_, rs.textRange))
//           .sequence: Option[List[Expr]]
//       } yield (newReturnStmt, List.empty[Expr])
//     case lv: LocalVar =>
//       ReferenceResolve.resolveTypeIfNeeded(lv.typ).orElse(ResolveRes.fromRes(lv.typ)).flatMap {
//         newType =>
//           // println(s"newtyp=${newType.text}, ${lv.name}, ${newType.getClass}")
//           lv.initExpr
//             .fold(ResolveRes.fromRes[Option[(Expr, List[Expr])]](None))(origExpr =>
//               resolveAndCheckExpr(origExpr, newType).map(Some.apply)
//             )
//             .map { case newExprAndProofs =>
//               (
//                 LocalVar(
//                   lv.modifiers,
//                   lv.varName,
//                   newType,
//                   newExprAndProofs.map(_._1),
//                   lv.endInd,
//                   lv.isFinal
//                 ),
//                 newExprAndProofs.fold(Nil)(_._2)
//               )
//             }
//       }
//     case block: Block =>
//       //Traverse the block, keeping track of the a Context holding new variable declarations
//       //and a ArrayBuffer containing resolved statements
//       val ctxtAndNewStmts: Option[(Context, mutable.ArrayBuffer[Statement])] =
//         block.stmts.foldLeft(
//           ResolveRes.fromRes(ctxt -> mutable.ArrayBuffer()): Option[
//             (Context, mutable.ArrayBuffer[Statement])
//           ]
//         ) { case (acc, stmt) =>
//           acc.flatMap { case context -> prev =>
//             // println(s"context proofdefs=${context.proofDefs.map(_.asInstanceOf[HasText].text)}")
//             resolveStmt(stmt, mthdReturnType, mthdProofs)(using context)
//               .orElse(
//                 ResolveRes.fromRes((stmt, Nil))
//               ) //If the statement could not be resolved, keep the old one
//               .map { (newStmt, newProofs) =>
//                 val updatedProofDefs = newProofs ::: context.proofDefs
//                 val newCtxt = newStmt match {
//                   case vd: LocalVar => //Add a new variable to _varRefs
//                     // println(s"just resolved ${vd.name}, ${vd.typ.text}, ${vd.typ.getClass}")
//                     val newGivenDefs =
//                       if (vd.isGiven) vd :: context.givenDefs
//                       else context.givenDefs

//                     val newProofDefs: List[ProofDef] =
//                       if (vd.isProof) vd :: updatedProofDefs
//                       else updatedProofDefs

//                     Context(
//                       context.varDefs + (vd.name -> vd),
//                       context.mthdDefs,
//                       newGivenDefs,
//                       newProofDefs,
//                       context.typeDefs,
//                       context.pkgDefs,
//                       context.cls,
//                       context.file
//                     )
//                   case _ => context.copy(proofDefs = updatedProofDefs)
//                 }
//                 newCtxt -> (prev += newStmt)
//               }
//           }
//         }

//       ctxtAndNewStmts.map((newCtxt, stmts) =>
//         // if (newCtxt.proofDefs.nonEmpty) println(s"new ctxt, proofs=${newCtxt.proofDefs.map(_.asInstanceOf[HasText].text)}")
//         (
//           Block(
//             stmts,
//             block.textRange,
//             if (stmts.isEmpty) VoidTypeRef(TextRange.synthetic) else statementType(stmts.last)
//           ),
//           Nil //newCtxt.proofDefs
//         )
//       )
//     case x => singleMsg(errorMsg("Did not resolve statement!", x))
//   }
// }

// /** Try to resolve an expression, and return an OptionT that hopefully contains a tuple holding the
//   * resolved expression and new proofs introduced by method calls
//   */
// private def resolveAndCheckExpr(
//   expr: Expr,
//   expectedType: Type
// )(using msgs: Messages, ctxt: Context): Option[(Expr, List[Expr])] = {
// //  logger.debug(s"Resolving expr ${expr.text}")

//   val resolved: Option[(Expr, List[Expr])] = expr match {
//     case AssignmentExpr(lhs, rhs, extraOp) =>
//       //TODO use extraOp
//       resolveAndCheckExpr(lhs, UnknownType).flatMapR { (newLhs, proofs) =>
//         resolveAndCheckExpr(rhs, newLhs.typ).map { (newRhs, proofs) =>
//           (AssignmentExpr(newLhs, newRhs, extraOp), proofs)
//         }
//       }
//     case FieldAccess(obj, fieldName) =>
//       resolveAndCheckExpr(obj, BuiltinTypes.objectType).flatMapR { (owner, proofs) =>
//         ReferenceResolve.findField(owner.typ, fieldName.text) match {
//           case Some(field) =>
//             ResolveRes.fromRes(FieldAccess(owner, field, fieldName.textRange) -> proofs)
//           case None => singleMsg(errorMsg(s"No such field ${fieldName.text}", fieldName))
//         }
//       }
//     case ctorCall: CtorCall => resolveUnresolvedCtorCall(ctorCall)
//     case mc: MethodCall     => resolveUnresolvedMethodCall(mc)
//     case ur.MultiDotRefExpr(path) =>
//       ReferenceResolve.resolveDotChainedRef(path).flatMapR {
//         case e: Expr => ResolveRes.fromRes((e, Nil))
//         case c       => singleMsg(errorMsg(s"${c.text} is a class, not an expression", c))
//       }
//     case b: BinaryExpr => resolveBinaryExpr(b)
//     case upe: ParenExpr =>
//       resolveAndCheckExpr(upe.expr, expectedType).map { (expr, proofs) =>
//         (ParenExpr(expr, upe.textRange), proofs)
//       }
//     case t: ThisRef => ResolveRes.fromRes(ThisRef(ctxt.cls, t.textRange) -> Nil)
//     case s: SuperRef =>
//       ctxt.cls match {
//         case cd: ClassDef =>
//           ResolveRes.fromRes(
//             SuperRef(
//               cd.superClass.asInstanceOf[ResolvedTypeRef].typeDef.asInstanceOf[Classlike],
//               s.textRange
//             ) -> Nil
//           )
//         case _ => singleMsg(errorMsg(s"${ctxt.cls.name} has no superclass", s))
//       }

//     case expr: Expr => ResolveRes.fromRes((expr, Nil))
//     case _ =>
//       singleMsg(errorMsg(s"aahh!! $expr wrong type!!! ${expr.getClass}", expr))
//   }

//   resolved.flatMapR { (expr, proofs) =>
//     if (
//       expectedType != UnknownType && expr.typ != UnknownType && !expr.typ.subTypeOf(expectedType)
//     ) {
//       /*println(
//         s"${expr.typ == expectedType}, ${expr.typ}, $expectedType, ${expectedType.getClass}, ${expr.typ.getClass}"
//       )*/
//       val msg =
//         s"${expr.text} is of wrong type: found ${expr.typ.text} (${expr.typ.getClass}), expected ${expectedType.text} (${expectedType.getClass})"
//       singleMsg(errorMsg(msg, expr))
//     } else {
//       // println(s"Type matched for ${expr.text}: ${expr.typ.text}!")
//       ResolveRes.fromRes((expr, proofs))
//     }
//   }
// }

// private def resolveUnresolvedCtorCall(
//   ctorCall: CtorCall
// )(using msgs: Messages, ctxt: Context): Option[(Expr, List[Expr])] = {
//   ReferenceResolve.resolveCls(ctorCall.cls.path, ctxt.typeDefs, ctxt.pkgDefs).flatMap { cls =>
//     ctorCall.typ = cls.makeRef
//     // ReferenceResolve.resolveDotChainedRef(mthdCall.asInstanceOf[CtorCall].cls.path)

//     resolveUnresolvedMethodCall(
//       ctorCall,
//       Some(ClassRef(cls, ctorCall.cls.path)),
//       possibleMthdsBasic(ctorCall, cls.methods.filter(_.isCtor)),
//       Nil,
//       isCtor = true
//     )
//   }
// }

// //TODO Deal with overloading first!!!
// private def resolveUnresolvedMethodCall(
//   mthdCall: MethodCall
// )(using ctxt: Context): Option[(Expr, List[Expr])] = {
//   val MethodCall(
//     _,
//     nameText @ Text(mthdName),
//     valArgs,
//     typeArgs,
//     givenArgs,
//     proofArgs
//   ) =
//     mthdCall

//   def resolveHelper(
//     caller: Option[Expr | ClassRef],
//     prevProofs: List[Expr]
//   )(using Messages): Option[(Expr, List[Expr])] = {
//     val allMthds = caller match {
//       case None              => ctxt.mthdDefs(mthdName).methods
//       case Some(e: Expr)     => e.typ.methods
//       case Some(c: ClassRef) => c.cls.methods
//     }
//     //Filter based on name and number of parameters
//     val possibleMthds = possibleMthdsBasic(mthdCall, allMthds.filter(_.name == mthdName))

//     resolveUnresolvedMethodCall(mthdCall, caller, possibleMthds, prevProofs, false)
//   }

//   (mthdCall.objOrCls: @unchecked) match {
//     case None => resolveHelper(None, Nil)
//     case Some(e: Expr) =>
//       resolveAndCheckExpr(e, BuiltinTypes.objectType).flatMap { (c, proofs) =>
//         resolveHelper(Some(c), proofs)
//       }
//     case Some(ur.MultiDotRef(path)) =>
//       ReferenceResolve.resolveDotChainedRef(path).flatMap { caller =>
//         resolveHelper(Some(caller), Nil)
//       }
//   }
// }

// /** Filter possible methods that a method call could be referring to (only by number of arguments,
//   * not name or types)
//   */
// private def possibleMthdsBasic(
//   mthdCall: UnresolvedMethodCall,
//   allMthds: Iterable[Method]
// )(using Messages): Iterable[Method] =
//   allMthds.filter(mthd =>
//     mthd.params.params.size == mthdCall.valArgs.args.size
//       && (mthdCall.typeArgs.isEmpty
//         || mthdCall.typeArgs.get.args.size == mthd.typeParams.params.size)
//       && (mthdCall.givenArgs.isEmpty
//         || mthd.givenParams.isEmpty
//         || mthdCall.givenArgs.get.args.size == mthd.givenParams.get.params.size)
//       && (mthdCall.proofArgs.isEmpty
//         || mthd.givenParams.isEmpty
//         || mthdCall.proofArgs.get.args.size == mthd.proofParams.get.params.size)
//   )

// private def resolveUnresolvedMethodCall(
//   mthdCall: MethodCall,
//   caller: Option[Expr | ClassRef],
//   possibleMthds: Iterable[Method],
//   prevProofs: List[Expr],
//   isCtor: Boolean
// )(using Messages, Context): Option[(Expr, List[Expr])] = {
// //  println(s"resolving method call ${mthdCall.text}, ${possibleMthds.map(_.name)}")
//   val nameText = mthdCall.methodName
//   val mthdName = nameText.text

//   if (possibleMthds.isEmpty) {
//     singleMsg(errorMsg(s"Cannot resolve method $mthdName", mthdCall))
//   } else if (possibleMthds.size > 1) {
//     singleMsg(errorMsg(s"Ambiguous method call $mthdName", mthdCall))
//   } else {
//     val resolvedMthd = possibleMthds.head
//     val argEndTextRange = TextRange.empty(mthdCall.valArgs.textRange.end)
//     val returnedProofs = resolvedMthd.proofs.map(Expr.dummy(_)).toList

//     for {
//       (newValArgs, newProofs) <- resolveArgList(
//         mthdCall.valArgs,
//         resolvedMthd.params.params.map(_.typ)
//       )
//       newGivenArgs <- ImplicitSearch.resolveImplicitArgList(
//         mthdCall.givenArgs,
//         resolvedMthd.givenParams,
//         argEndTextRange
//       )
//       newProofArgs <- ImplicitSearch.resolveImplicitArgList(
//         mthdCall.proofArgs,
//         resolvedMthd.proofParams,
//         argEndTextRange
//       )
//     } yield {
//       val resolvedCall =
//         if (!isCtor)
//           MethodCall(
//             caller,
//             nameText,
//             newValArgs,
//             mthdCall.typeArgs,
//             newGivenArgs,
//             newProofArgs,
//             resolvedMthd.returnType,
//             resolvedMthd
//           )
//         else
//           CtorCall(
//             caller.get.asInstanceOf[ClassRef],
//             newValArgs,
//             mthdCall.typeArgs,
//             newGivenArgs,
//             newProofArgs,
//             resolvedMthd.returnType,
//             resolvedMthd,
//             Position.synthetic
//           )
//       // println(s"resolvedmthd=${resolvedMthd.name}, proofs=${resolvedMthd.proofs.map(_.text)}")
//       (resolvedCall, returnedProofs ::: newProofs ::: prevProofs)
//     }
//   }
// }

// private def resolveArgList(
//   origArgs: ArgList,
//   expectedTypes: Iterable[Type]
// )(using Messages, Context): Option[(ArgList, List[Expr])] = {
// //  println(s"resolving arglist ${origArgs.text}, ${expectedTypes.map(_.text)}")
//   //Resolves all arguments, and keeps track of whether or not all of them were resolved
//   val resolvedArgs: ResLogged[(List[(Expr, List[Expr])], Boolean)] =
//     origArgs.args
//       .lazyZip(expectedTypes)
//       .foldLeft(
//         ResLogged[
//           (List[(Expr, List[Expr])], Boolean)
//         ]((Nil, false), Nil)
//       ) { case (acc, (origArg, expectedType)) =>
//         acc.flatMap { case (prevArgs, allResolved) =>
//           resolveAndCheckExpr(origArg, expectedType)
//             .map(resolved => (resolved :: prevArgs, allResolved))
//             .getOrElse(((origArg, Nil) :: prevArgs, false)): ResLogged[
//             (List[(Expr, List[Expr])], Boolean)
//           ]
//         }
//       }

//   resolvedArgs.map { case (resolvedArgs, allResolved) =>
//     //TODO do something with allResolved to signal that everything is not okay
//     val (args, proofs) = resolvedArgs.reverse.unzip
//     Some((ArgList(args, origArgs.argsKind, origArgs.textRange), proofs.flatten))
//   }.toResolveRes
// }

// private def resolveBinaryExpr(
//   expr: BinaryExpr
// )(using Messages, Context): Option[(Expr, List[Expr])] = {
//   val BinaryExpr(lhs, op, rhs) = expr
//   import OpType._
//   op.opType match {
//     case SUBTRACT | MULTIPLY | DIVIDE =>
//       for {
//         (newLhs, lhsProofs) <- resolveAndCheckExpr(lhs, PrimitiveTypeDef.numericType)
//         (newRhs, rhsProofs) <- resolveAndCheckExpr(rhs, PrimitiveTypeDef.numericType)
//       } yield (BinaryExpr(newLhs, op, newRhs, ???), lhsProofs ::: rhsProofs)
//     case _ => ResolveRes.fromLogs(errorMsg("Operator not implemented", op) :: Nil)
//   }
// }

// private def statementType(stmt: Statement): Type = stmt match {
//   case ht: HasType => ht.typ
//   case _           => VoidTypeRef(TextRange.synthetic)
// }
