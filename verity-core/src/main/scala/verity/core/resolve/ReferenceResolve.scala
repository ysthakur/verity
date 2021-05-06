package verity.core.resolve

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context}
import Package.Importable
import Context.Refs

import com.typesafe.scalalogging.Logger

import annotation.tailrec

private[verity] object ReferenceResolve {
  def resolveType(typ: TypeRef)(using ctxt: Context): Type =
    ???

  private[resolve] def resolveDotChainedRef(path: Seq[Text])(using ctxt: Context): Either[Text, Expr | ClassRef] = {
    val head +: tail = path

    ctxt.varRefs.find(_._1 == head.text) match {
      case Some(decl) => resolveExprOrCls(VarRef(head, decl._2), tail)
      case None =>
        ctxt.clsRefs.find(_._1 == head.text) match {
          case Some(cls) => resolveExprOrCls(ClassRef(head, cls._2, None), tail)
          case None =>
            ctxt.pkgRefs.find(_._1 == head.text) match {
              case Some(pkg) => resolveExprOrCls(pkg._2, head :: Nil, tail)
              case None => Left(head)
            }
        }
    }
  }

  @tailrec
  private def resolveExprOrCls(
      prev: Expr,
      path: Seq[Text]
  ): Either[Text, Expr | ClassRef] = path match {
    case head +: tail =>
      prev.exprType.fields.find(_.name == head.text) match {
        case Some(field) => resolveExprOrCls(FieldAccess(prev, field, head.textRange), tail)
        case None        => Left(head)
      }
    case _ => Right(prev)
  }

  private def resolveExprOrCls(
      prev: ClassRef,
      path: Seq[Text]
  ): Either[Text, Expr | ClassRef] = path match {
    case head +: tail =>
      prev.cls.fields.find(f => f.isStatic && f.name == head.text) match {
        case Some(field) => resolveExprOrCls(StaticFieldAccess(prev, field, head.textRange), tail)
        case None => Left(head)
      }
    case _ => Right(prev)
  }

  @tailrec
  private def resolveExprOrCls(
      prev: Package,
      prevPath: List[Text],
      path: Seq[Text]
  ): Either[Text, Expr | ClassRef] = path match {
    case head +: tail =>
      prev.classlikes.find(_.name == head.text) match {
        case Some(cls) => resolveExprOrCls(ClassRef(head, cls, Some(PkgRef(prevPath, prev))), tail)
        case None =>
          prev.subPkgs.find(_.name == head.text) match {
            case Some(pkg) => resolveExprOrCls(pkg, head :: prevPath, tail)
            case None      => Left(head)
          }
      }
    case _ => Left(prevPath.head)
  }

  def resolveImports(
      imports: Iterable[ImportStmt],
      file: FileNode
  )(using rootPkg: RootPkg, logger: Logger): Iterable[(String, Importable, ImportStmt)] =
    imports.view.flatMap { case imptStmt @ ImportStmt(DotPath(dotPath), _, wildcard) =>
      val path = dotPath.view.map(_._1)
      Package
        .findImptableAbs(path)
        .fold {
          logger.error(s"Not found $dotPath")
          Nil
        } { impt =>
          if (!wildcard) {
            Seq((path.last, impt, imptStmt))
          } else {
            impt match {
              case pkg: Package =>
                pkg.subPkgs.view.map(p => (p.name, p, imptStmt))
                  ++ pkg.classlikes.map(c => (c.name, c, imptStmt))
              case cls: Classlike =>
                cls.importableChildren.map(c => (c.name, c, imptStmt))
              case _ =>
                Compiler.logError(s"Cannot import members of ${impt.name}", imptStmt, file)
                Nil
            }
          }
        }
    }
}
