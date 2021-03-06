package verity.core.resolve

import verity.ast._
import verity.ast.infile.{unresolved => ur, _}
import verity.core._

import cats.Traverse
import cats.data.{OptionT, Writer}
import cats.implicits._
import cats.syntax.traverse._

object ImplicitSearch {

  private[resolve] def resolveImplicitArgList(
    argList: Option[ur.UnresolvedArgList],
    params: Option[ParamList],
    defaultTextRange: TextRange
  )(using ctxt: Context): ResolveResult[Option[ArgList]] = {
    // println(s"givenDefs=${ctxt.givenDefs.map(_.asInstanceOf[HasText].text)}, proofDefs=${ctxt.proofDefs.map(_.asInstanceOf[HasText].text)}")
    argList match {
      case Some(origArgs) =>
        params match {
          case Some(paramList) =>
            resolveArgList(origArgs, paramList.params.map(_.typ)).map{ case (args, _) => Some(args) }
          case None =>
            singleMsg(errorMsg(s"No ${origArgs.argsKind} arguments expected", origArgs.asInstanceOf))
        }
      case None =>
        params match {
          case Some(paramList) =>
            (paramList.kind: @unchecked) match {
              case ParamListKind.GIVEN =>
                OptionT(paramList.params
                  .map(param => findGiven(param.typ, defaultTextRange).getOrElse(ur.UnresolvedImplicit(param)))
                  .sequence
                  .map(givenArgs => Some(Some(ArgList(givenArgs, ArgsKind.Given, TextRange.synthetic)))))
              case ParamListKind.PROOF =>
                OptionT(paramList.params
                  .map(param => findProof(param.typ, defaultTextRange).getOrElse(ur.UnresolvedImplicit(param)))
                  .sequence
                  .map(proofArgs => Some(Some(ArgList(proofArgs, ArgsKind.Proof, TextRange.synthetic)))))
            }
          case None            => OptionT(Writer(Nil, Some(None)))
        }
    }
  }
  
  //TODO reduce code duplication (findGiven and findProof)
  def findGiven(givenType: Type, tr: TextRange)(using ctxt: Context): ResolveResult[Expr] =
    val poss = findGivens(givenType)

    if (poss.isEmpty) {
      singleMsg(errorMsg(s"No givens of type ${givenType.text} found", tr))
    } else if (poss.size > 1) {
      singleMsg(
        errorMsg(
          s"Ambiguous implicits - all of the following match type ${givenType.text}:\n${poss.map(_.text).mkString("\n")}",
          tr
        )
      )
    } else {
      OptionT.some(poss.head)
    }

  def findProof(proofType: Type, tr: TextRange)(using ctxt: Context): ResolveResult[Expr] = {
    val poss = findProofs(proofType)

    if (poss.isEmpty) {
      singleMsg(errorMsg(s"No proofs of type ${proofType.text} found", tr))
    } else if (poss.size > 1) {
      singleMsg(
        errorMsg(
          s"Ambiguous implicits - all of the following match type ${proofType.text}:\n${poss.map(_.text).mkString("\n")}",
          tr
        )
      )
    } else {
      OptionT.some(poss.head)
    }
  }

  def findGivens(givenType: Type)(using ctxt: Context): Iterable[Expr] =
    ctxt.givenDefs.view.map {
      case vd: VariableDecl =>
        // println(s"vd=${vd.text}, vd.typ=${vd.typ.text}, vdtypcls=${vd.typ.getClass}")
        Option.when(vd.typ.subTypeOf(givenType))(VarRef(Text(vd.name), vd))
      case mthd: Methodlike => ???
    }.collect {
      case Some(expr) => expr
    }

  def findProofs(proofType: Type)(using ctxt: Context): Iterable[Expr] = {
    proofType match {
      case ResolvedTypeRef(_, TypeArgList(singleArg, _), NotGivenDef) =>
        // println(s"finding notgiven, ${singleArg.head.text}")
        //There shouldn't be a given of type singleArg.head (proofType is NotGivenDef<singleArg.head>)
        val givens = findGivens(singleArg.head)
        if (givens.isEmpty) Seq(NullLiteral(TextRange.synthetic))
        else Nil
      case ResolvedTypeRef(_, TypeArgList(singleArg, _), NotProvenDef) =>
        val proofs = findProofs(singleArg.head)
        val res = if (proofs.isEmpty) Seq(NullLiteral(TextRange.synthetic))
          else Nil
        // println(s"finding notproven, ${singleArg.head.text}, res=${res.map(_.asInstanceOf[HasText].text)}")
        res
      case _ =>
        val res = (ctxt.proofDefs.view ++ ctxt.givenDefs).map {
          case expr: Expr =>
            // println(s"checking if ${expr.text} subtype of ${proofType.text}, ${expr.typ.subTypeOf(proofType)}, ${expr.typ.getClass}, ${proofType.getClass}")
            Option.when(expr.typ.subTypeOf(proofType))(expr)
          case vd: VariableDecl =>
            // println(s"vd=${vd.text}, vd.typ=${vd.typ.text}, vdtypcls=${vd.typ.getClass}")
            Option.when(vd.typ.subTypeOf(proofType))(VarRef(Text(vd.name), vd))
          case mthd: Methodlike => ???
        }.collect {
          case Some(expr) => expr
        }.toSeq
        // println(s"finding proofs, ${proofType.text}, ${res.map(_.asInstanceOf[HasText].text)}")
        res
    }
  }
}
