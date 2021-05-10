package verity.core.resolve

import com.typesafe.scalalogging.Logger
import verity.ast._
import verity.ast.infile._
import verity.ast.infile.unresolved.UnresolvedTypeRef
import verity.core._
import verity.core.Context.Defs
import verity.checks.CheckTypes
import verity.ast.Pkg.Importable

import cats.data.{OptionT, Writer}
import cats.implicits._
import cats.catsInstancesForId

import scala.annotation.tailrec

private[verity] object ReferenceResolve {
  def resolveTypeIfNeeded(typ: Type)(using ctxt: Context): ResolveResult[Type] =
    resolveTypeIfNeeded(typ, ctxt.typeDefs, ctxt.pkgDefs)

  def resolveTypeIfNeeded(
    typ: Type,
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg]
  ): ResolveResult[Type] = typ match {
    case (_: PrimitiveType) | (_: VoidTypeRef) => OptionT.some(typ)
    case tr: UnresolvedTypeRef                 => resolveTypeRef(tr, typeDefs, pkgDefs)
    case _                                     => ???
  }

  def resolveTypeRef(typ: UnresolvedTypeRef)(using ctxt: Context): ResolveResult[Type] =
    resolveTypeRef(typ, ctxt.typeDefs, ctxt.pkgDefs)

  def resolveTypeRef(
    typ: UnresolvedTypeRef,
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg]
  ): ResolveResult[Type] = {
    val typeArgsRange = typ.args.textRange
    val resolvedCls = resolveCls(typ.path, typeDefs, pkgDefs).value

    //A writer where the value is a tuple (areAllArgsResolved?, resolvedArgsInReverse)
    val resolvedArgs: ResultWithLogs[(Boolean, List[Type])] =
      typ.args.args.foldLeft(Writer(List.empty[CompilerMsg], true -> List.empty[Type])) {
        (acc, arg) =>
          acc.flatMap { case allResolved -> prev =>
            resolveTypeIfNeeded(arg, typeDefs, pkgDefs)
              .map(typ => allResolved -> (typ :: prev))
              .getOrElse(false -> (arg :: prev))
          }
      }

    OptionT(
      for {
        value <- resolvedArgs
        maybeCls <- resolvedCls
        (allResolved, reversedArgs) = value
        args = reversedArgs.reverse
        _ <- Writer.tell(
          //Log errors with the arguments if the class is resolved
          maybeCls.fold(Nil)(cls =>
            CheckTypes.checkTypeArgs(args, cls.typeDef.typeParams.params, typeArgsRange)
          )
        )
        argList = TypeArgList(args, typ.args.textRange)
        res <- Writer.value(
          Some(
            //Only return a ResolvedTypeRef if the class and all arguments are resolved
            if (allResolved && maybeCls.nonEmpty)
              ResolvedTypeRef(typ.path, argList, maybeCls.get.typeDef)
            else
              UnresolvedTypeRef(typ.path, argList, maybeCls.map(_.typeDef))
          )
        )
      } yield res
    )

  }

  private[verity] def findField(typ: Type, fieldName: String): Option[Field] =
    typ.fields.find(_.name == fieldName)

  private[resolve] def resolveDotChainedRef(
    path: Seq[Text]
  )(using ctxt: Context): ResolveResult[Expr | ClassRef] = {
    val head +: tail = path
    ctxt.varDefs.find(_._1 == head.text) match {
      case Some(decl) =>
        resolveExprOnly(VarRef(head, decl._2), tail).asInstanceOf[ResolveResult[Expr | ClassRef]]
      case None =>
        ctxt.typeDefs.find(_._1 == head.text) match {
          case Some((_, cls: Classlike)) =>
            resolveExprOrCls(ClassRef(cls, List(head)), tail)
          case _ =>
            ctxt.pkgDefs.find(_._1 == head.text) match {
              case Some(pkg) => resolveExprOrCls(pkg._2, head :: Nil, tail)
              case None      => singleMsg(errorMsg(s"Symbol ${head.text} not found", head.textRange))
            }
        }
    }
  }

  def resolveCls(
    path: Seq[Text],
    typeDefs: Defs[TypeDef],
    pkgDefs: Defs[Pkg]
  ): ResolveResult[ResolvedTypeRef] = {
    val head +: tail = path
    typeDefs.find(_._1 == head.text) match {
      case Some((_, cls: Classlike)) =>
        resolveInnerCls(cls, List(head), tail)
      case _ =>
        pkgDefs.find(_._1 == head.text) match {
          case Some(_ -> pkg) => resolveClsInPkg(pkg, head :: Nil, tail)
          case None           => singleMsg(errorMsg(s"Symbol ${head.text} not found", head.textRange))
        }
    }
  }

  @tailrec
  private def resolveClsInPkg(
    prev: Pkg,
    prevPath: List[Text],
    path: Seq[Text]
  ): ResolveResult[ResolvedTypeRef] = path match {
    case head +: tail =>
      prev.classlikes.find(_.name == head.text) match {
        case Some(cls) =>
          resolveInnerCls(cls, head :: prevPath, tail)
        case None =>
          prev.subPkgs.find(_.name == head.text) match {
            case Some(pkg) => resolveClsInPkg(pkg, head :: prevPath, tail)
            case None =>
              singleMsg(
                errorMsg(
                  s"${head.text} is not a member of package ${HasText.seqText(prevPath.reverse, ".")}",
                  head.textRange
                )
              )
          }
      }
    case _ =>
      singleMsg(
        errorMsg(
          s"${prev.name} is a package, not a class",
          prevPath.head.textRange
        )
      )
  }

  //todo deal with inner classes?
  private def resolveInnerCls(
    prev: Classlike,
    prevPath: List[Text],
    path: Seq[Text]
  ): ResolveResult[ResolvedTypeRef] = path match {
    case head +: tail => singleMsg(errorMsg("No inner classes yet", head.textRange))
    case _            => OptionT(Writer(Nil, Some(prev.makeRef(prevPath.reverse))))
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
          resolveExprOrCls(ClassRef(cls, prevPath.reverse), tail)
        case None =>
          prev.subPkgs.find(_.name == head.text) match {
            case Some(pkg) => resolveExprOrCls(pkg, head :: prevPath, tail)
            case None =>
              singleMsg(
                errorMsg(
                  s"${head.text} is not a member of package ${HasText.seqText(prevPath.reverse, ".")}",
                  head.textRange
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
          singleMsg(
            errorMsg(s"Field ${head.text} not found in class ${prev.cls.name}", head.textRange)
          )
      }
    case _ => OptionT.some(prev)
  }

  def mergeResults[T](results: List[ResultWithLogs[T]]): ResultWithLogs[List[T]] =
    results.traverse(Predef.identity)
}
