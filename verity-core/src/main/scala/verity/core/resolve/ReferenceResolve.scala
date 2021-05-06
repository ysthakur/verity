package verity.core.resolve

import com.typesafe.scalalogging.Logger
import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context}
import verity.core.Context.Defs
import verity.ast.Pkg.Importable

import scala.annotation.tailrec

private[verity] object ReferenceResolve {
  def resolveType(typ: TypeRef)(using ctxt: Context): Type =
    ???

  private[verity] def findField(typ: Type, fieldName: String): Option[Field] =
    typ.fields.find(_.name == fieldName)

  private[resolve] def resolveDotChainedRef(
      path: Seq[Text]
  )(using ctxt: Context): Either[(String, TextRange), Expr | ClassRef] = {
    val head +: tail = path

    ctxt.varDefs.find(_._1 == head.text) match {
      case Some(decl) => resolveExprOnly(VarRef(head, decl._2), tail)
      case None =>
        ctxt.typeDefs.find(_._1 == head.text) match {
          case Some((_, cls: Classlike)) => resolveExprOrCls(ClassRef(cls, None, head.textRange), tail)
          case _ =>
            ctxt.pkgDefs.find(_._1 == head.text) match {
              case Some(pkg) => resolveExprOrCls(pkg._2, head :: Nil, tail)
              case None      => Left(s"Symbol ${head.text} not found" -> head.textRange)
            }
        }
    }
  }

  @tailrec
  private def resolveExprOrCls(
      prev: Pkg,
      prevPath: List[Text],
      path: Seq[Text]
  ): Either[(String, TextRange), Expr | ClassRef] = path match {
    case head +: tail =>
      prev.classlikes.find(_.name == head.text) match {
        case Some(cls) => resolveExprOrCls(ClassRef(cls, Some(PkgRef(prevPath, prev)), head.textRange), tail)
        case None =>
          prev.subPkgs.find(_.name == head.text) match {
            case Some(pkg) => resolveExprOrCls(pkg, head :: prevPath, tail)
            case None =>
              Left(
                  s"${head.text} is not a member of package ${HasText.seqText(prevPath.reverse, ".")}" -> head.textRange
              )
          }
      }
    case _ =>
      Left(s"${prev.name} is a package, not an expression or class" -> prevPath.head.textRange)
  }

  /**
   * Find a field of an object (or one of its field's fields, ...)
   */
  @tailrec
  private def resolveExprOnly(
      prev: Expr,
      path: Seq[Text]
  ): Either[(String, TextRange), Expr] = path match {
    case head +: tail =>
      prev.exprType.fields.find(_.name == head.text) match {
        case Some(field) => resolveExprOnly(FieldAccess(prev, field, head.textRange), tail)
        case None        => Left(s"No field named ${head.text} found" -> head.textRange)
      }
    case _ => Right(prev)
  }

  /**
   * Find a field in a class (or one of its field's fields, ...)
   */
  private def resolveExprOrCls(
      prev: ClassRef,
      path: Seq[Text]
  ): Either[(String, TextRange), Expr | ClassRef] = path match {
    case head +: tail =>
      prev.cls.fields.find(f => f.isStatic && f.name == head.text) match {
        case Some(field) => resolveExprOnly(StaticFieldAccess(prev, field, head.textRange), tail)
        case None        => Left(s"Field ${head.text} not found in class ${prev.cls.name}" -> head.textRange)
      }
    case _ => Right(prev)
  }

  /**
   *
   * Resolve imports and return a list of `(<name of pkg or cls, pkg or cls, import stmt it came from>)`
   */
  private[verity] def resolveImports(
                      imports: Iterable[ImportStmt],
                      file: FileNode
                    )(using rootPkg: RootPkg, logger: Logger): Iterable[(String, Importable, ImportStmt)] =
    imports.view.flatMap { case imptStmt @ ImportStmt(DotPath(dotPath), _, wildcard) =>
      val path = dotPath.view.map(_._1)
      Pkg
        .findImptableAbs(path)
        .fold {
          logger.error(s"Not found $dotPath")
          Nil
        } { impt =>
          if !wildcard then {
            Seq((path.last, impt, imptStmt))
          } else {
            impt match {
              case pkg: Pkg =>
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
