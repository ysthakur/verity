package verity.checks

import verity.ast._
import verity.ast.infile._
import verity.util._
import verity.core.Context.Defs
import verity.core.resolve.ReferenceResolve
import verity.core._
import verity.ast.Pkg.Importable
import com.typesafe.scalalogging.Logger

import scala.collection.mutable

//todo move into verity.core?
object InitialPass {

  /** Resolve method and field types
    * @param pkg The package to work on
    * @param parentPkgs A list of this package's parents (in reverse)
    * @param logger The logger to use
    */
  def initialPass(root: RootPkg)(using logger: Logger): Unit = {
    given RootPkg = root
    verity.core.PackageUtil.walkWithPath(
      root,
      (file, parentPkgs, pkgName) => if (file.isSource) initialPassFile(file, parentPkgs, pkgName)
    )
  }

  /** Resolve all references to classes and type parameters in a file
    * @param file The file to work on
    * @param parentPkgs A non-empty list of this package's parents (in reverse)
    * @param root The root package
    * @param logger The logger to use
    */
  private def initialPassFile(
    file: FileNode,
    parentPkgs: List[Pkg],
    pkgName: String
  )(using rootPkg: RootPkg, logger: Logger): Unit = {
    val currPkg = parentPkgs.head
    val FileNode(name, pkgRef, imports, _, _) = file

    verifyPkgStmt(pkgRef, pkgName, name)

    val resolvedImports = resolveImports(imports, file)

    val pkgMap = mutable.HashMap[String, Pkg]()
    val clsMap = mutable.HashMap[String, Classlike]()

    //todo find a way to reduce code duplication
    pkgMap.addAll(rootPkg.subPkgs.view.map(p => p.name -> p))
    clsMap.addAll(currPkg.classlikes.map(c => c.name -> c))

    file.resolvedImports = resolvedImports.map { case (name, imported, imptStmt) =>
      imported match {
        case pkg: Pkg =>
          if pkgMap.contains(name) then {
            LogUtils.logMsg(
              s"Cannot import package $name: Pkg of same name already in scope",
              imptStmt,
              file
            )
          } else {
            pkgMap += name -> pkg
          }
        case cls: Classlike =>
          if clsMap.contains(name) then {
            LogUtils.logMsg(
              s"Cannot import class ${name}: class of same name already in scope",
              imptStmt,
              file
            )
          } else {
            clsMap += name -> cls
          }
      }

      imported.asInstanceOf[Pkg.Importable]
    }

    val pkgIMap = pkgMap.toMap
    val clsIMap = clsMap.toMap

    file.classlikes.foreach(c =>
      initialPassCls(c, clsIMap, pkgIMap, file).foreach(LogUtils.log(_, file))
    )
  }

  //todo check modifiers and stuff
  /** Perform an initial pass over this class, resolving references and finding non-type-related
    * errors.
    *
    * @param cls
    *   The current class
    * @param pkgMap
    *   A map of packages that are visible outside
    * @param clsMap
    *   A map of classes that are visible outside
    * @param file
    *   The current file
    */
  private def initialPassCls(
    cls: Classlike,
    clsRefs: Defs[Classlike],
    pkgRefs: Defs[Pkg],
    file: FileNode
  )(using logger: Logger): Iterable[CompilerMsg] = {
    cls match {
      case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
      case _                              =>
    }

    //This context is purely for ReferenceResolve.resolveType
    val dummyCtxt: Context = Context(Map.empty, Map.empty, Nil, Nil, clsRefs, pkgRefs, cls, file)

    cls.methods.flatMap { mthd =>
      mthd match {
        case c: Constructor =>
          val mthdName = mthd.name
          if mthdName != cls.name && mthdName != Keywords.constructorName then {
            LogUtils.logMsg(s"Wrong constructor name: $mthdName", mthd.nameRange, file)
          }
        case m: NormMethod => //TODO!!!!!!!!!!!!!!!!!!!1
//          m.returnType = ReferenceResolve.resolveTypeIfNeeded(mthd.returnType)(using dummyCtxt)
      }

      initialPassMthd(mthd, clsRefs, pkgRefs, cls, file)
    }
  }

  private def initialPassMthd(
    mthd: Method,
    clsRefs: Defs[Classlike],
    pkgRefs: Defs[Pkg],
    cls: Classlike,
    file: FileNode
  )(using Logger): Iterable[CompilerMsg] = {
    val isCtor = mthd.isInstanceOf[Constructor]
    given Context = Context(
      cls.fields.view.map(f => f.name -> f).toMap,
      cls.methods
        .groupBy(_.name)
        .view
        .mapValues(mthds => MethodGroup(mthds.head.name, mthds))
        .toMap,
      cls.givenChildren,
      cls.proofChildren,
      clsRefs,
      pkgRefs,
      cls,
      file
    )
    val bodyRes = mthd.body match {
      case Some(_) =>
        mthd.modifiers.find(_.modType == ModifierType.ABSTRACT) match {
          case Some(mod) =>
            errorMsg("Method with implementation cannot be abstract", mod) :: Nil
          case None =>
            if (isCtor) {
              //TODO check if first statement is call to this() or super()

            }
            Nil
        }
      case None =>
        if (!mthd.isAbstract)
          errorMsg("Method requires abstract modifier or implementation", mthd.nameRange) :: Nil
        else Nil
    }
    //TODO resolve return type
    mthd match {
      case nm: NormMethod =>
        bodyRes ::: ReferenceResolve
          .resolveTypeIfNeeded(mthd.returnType)
          .map { typ =>
            nm.returnType = typ
          }
          .value
          .written
      case _ => bodyRes
    }
  }

  private def verifyPkgStmt(
    pkgRef: Option[PackageStmt],
    pkgName: String,
    fileName: String
  ): Option[String] =
    pkgRef match {
      case Some(pkgStmt) =>
        val foundText = pkgStmt.path.text
        Option.when(foundText != pkgName && pkgName.nonEmpty)(
          s"Wrong package statement in file ${fileName}, should be $pkgName, found $foundText"
        )
      case None =>
        Option.when(pkgName.nonEmpty)(
          s"No package statement in file ${fileName} in package $pkgName"
        )
    }

  /** Resolve imports and return a list of `(<name of pkg or cls, pkg or cls, import stmt it came from>)`
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
                LogUtils.logMsg(s"Cannot import members of ${impt.name}", imptStmt, file)
                Nil
            }
          }
        }
    }
}
