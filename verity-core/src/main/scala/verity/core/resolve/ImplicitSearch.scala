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
    println(s"givenDefs=${ctxt.givenDefs.map(_.asInstanceOf[HasText].text)}")
    argList match {
      case Some(origArgs) =>
        params match {
          case Some(paramList) =>
            resolveArgList(origArgs, paramList.params.map(_.typ)).map(Some.apply)
          case None =>
            singleMsg(errorMsg(s"No ${origArgs.argsKind} arguments expected", origArgs.asInstanceOf))
        }
      case None =>
        params match {
          case Some(paramList) =>
            (paramList.kind: @unchecked) match {
              case ParamListKind.GIVEN =>
                OptionT(paramList.params
                  .map(param => findGiven(param, defaultTextRange).getOrElse(ur.UnresolvedImplicit(param)))
                  .sequence
                  .map(givenArgs => Some(Some(ArgList(givenArgs, ArgsKind.Given, TextRange.synthetic)))))
              case ParamListKind.PROOF => ???
            }
          case None            => OptionT(Writer(Nil, Some(None)))
        }
    }
  }
  
  def findGiven(givenParam: Parameter, tr: TextRange)(using ctxt: Context): ResolveResult[Expr] =
    val poss = ctxt.givenDefs.view.map {
      case vd: VariableDecl =>
        println(s"vd=${vd.text}, vd.typ=${vd.typ.text}, vdtypcls=${vd.typ.getClass}")
        Option.when(vd.typ.subTypeOf(givenParam.typ))(VarRef(Text(vd.name), vd))
      case mthd: Methodlike => ???
    }.collect {
      case Some(expr) => expr
    }

    if (poss.isEmpty) {
      singleMsg(errorMsg(s"No givens of type ${givenParam.typ.text} found", tr))
    } else if (poss.size > 1) {
      singleMsg(
        errorMsg(
          s"Ambiguous implicits - all of the following match type ${givenParam.typ.text}: ${poss.map(_.text).mkString("\n")}",
          tr
        )
      )
    } else {
      OptionT.some(poss.head)
    }

  def findProof(proofParam: Parameter)(using ctxt: Context): ResolveResult[Expr] =
    ???

}
