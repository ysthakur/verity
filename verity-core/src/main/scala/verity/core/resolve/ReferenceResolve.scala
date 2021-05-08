package verity.core.resolve

import com.typesafe.scalalogging.Logger
import verity.ast._
import verity.ast.infile._
import verity.core.{Compiler, Context, errorMsg, ResolveResult, singleMsg}
import verity.core.Context.Defs
import verity.ast.Pkg.Importable

import cats.data.{OptionT, Writer}
import cats.implicits._

import scala.annotation.tailrec

private[verity] object ReferenceResolve {
  def resolveTypeIfNeeded(typ: Type)(using ctxt: Context): ResolveResult[Type] = typ match {
    case pt: PrimitiveType => OptionT.some(pt)
    case tr: ResolvedTypeRef => resolveTypeRef(tr)
    case _ => ???
  }

  def resolveTypeRef(typ: ResolvedTypeRef)(using ctxt: Context): ResolveResult[Type] =
    resolveTypeRef(typ, ctxt.typeDefs, ctxt.pkgDefs)

  def resolveTypeRef(typ: ResolvedTypeRef, typeDefs: Defs[TypeDef], pkgDefs: Defs[Pkg]): ResolveResult[Type] = {
    ???
  }

  private[verity] def findField(typ: Type, fieldName: String): Option[Field] =
    typ.fields.find(_.name == fieldName)

  private[resolve] def resolveDotChainedRef(
      path: Seq[Text]
  )(using ctxt: Context): ResolveResult[Expr | ClassRef] = {
    val head +: tail = path
    ctxt.varDefs.find(_._1 == head.text) match {
      case Some(decl) => resolveExprOnly(VarRef(head, decl._2), tail).asInstanceOf[ResolveResult[Expr | ClassRef]]
      case None =>
        ctxt.typeDefs.find(_._1 == head.text) match {
          case Some((_, cls: Classlike)) =>
            resolveExprOrCls(ClassRef(cls, None, head.textRange), tail)
          case _ =>
            ctxt.pkgDefs.find(_._1 == head.text) match {
              case Some(pkg) => resolveExprOrCls(pkg._2, head :: Nil, tail)
              case None      => singleMsg(errorMsg(s"Symbol ${head.text} not found", head.textRange))
            }
        }
    }
  }

  @tailrec
  private def resolveExprOrCls(
      prev: Pkg,
      prevPath: List[Text],
      path: Seq[Text]
  ): ResolveResult[Expr | ClassRef] = path match {
    case head +: tail =>
      prev.classlikes.find(_.name == head.text) match {
        case Some(cls) =>
          resolveExprOrCls(ClassRef(cls, Some(PkgRef(prevPath, prev)), head.textRange), tail)
        case None =>
          prev.subPkgs.find(_.name == head.text) match {
            case Some(pkg) => resolveExprOrCls(pkg, head :: prevPath, tail)
            case None =>
              singleMsg(
                  errorMsg(
                      s"${head.text} is not a member of package ${HasText
                        .seqText(prevPath.reverse, ".")}", head.textRange
                  )
              )
          }
      }
    case _ =>
      singleMsg(
          errorMsg(
              s"${prev.name} is a package, not an expression or class",
              prevPath.head.textRange
          )
      )
  }

  /** Find a field of an object (or one of its field's fields, ...)
    */
  @tailrec
  private def resolveExprOnly(
      prev: Expr,
      path: Seq[Text]
  ): ResolveResult[Expr] = path match {
    case head +: tail =>
      prev.typ.fields.find(_.name == head.text) match {
        case Some(field) => resolveExprOnly(FieldAccess(prev, field, head.textRange), tail)
        case None        => singleMsg(errorMsg(s"No field named ${head.text} found", head.textRange))
      }
    case _ => OptionT.some(prev)
  }

  /** Find a field in a class (or one of its field's fields, ...)
    */
  private def resolveExprOrCls(
      prev: ClassRef,
      path: Seq[Text]
  ): ResolveResult[Expr | ClassRef] = path match {
    case head +: tail =>
      prev.cls.fields.find(f => f.isStatic && f.name == head.text) match {
        case Some(field) =>
          resolveExprOnly(StaticFieldAccess(prev, field, head.textRange), tail)
            .asInstanceOf[ResolveResult[Expr | ClassRef]]
        case None =>
          singleMsg(errorMsg(s"Field ${head.text} not found in class ${prev.cls.name}", head.textRange))
      }
    case _ => OptionT.some(prev)
  }
}
