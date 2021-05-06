package verity.checks

import verity.ast.*
import verity.ast.infile.*
import verity.core.{Compiler, Context, Keywords}
import verity.core.resolve.ReferenceResolve
import verity.util.*
import verity.checks.InitialChecks
import Pkg.Importable
import Context.Defs

import com.typesafe.scalalogging.Logger

import scala.collection.mutable.HashMap
//todo move into verity.core?
object InitialChecks {

  /** Resolve method and field types
    * @param pkg The package to work on
    * @param parentPkgs A list of this package's parents (topmost packages at the end)
    * @param logger The logger to use
    */
  def initialPass(root: RootPkg)(using logger: Logger): Unit = {
    given RootPkg = root
    root.walkWithPath(initialPassFile)
  }

  /** Resolve all references to classes and type parameters in a file
    * @param file The file to work on
    * @param parentPkgs A non-empty list of this package's parents (topmost packages at the end)
    * @param root The root package
    * @param logger The logger to use
    */
  private def initialPassFile(
      file: FileNode,
      parentPkgs: List[Pkg],
      pkgName: String
  )(using rootPkg: RootPkg, logger: Logger): Unit = {
    val currPkg = parentPkgs.head
    val FileNode(name, pkgRef, imports, classlikes, jFile) = file

    InitialChecks.verifyPkgStmt(pkgRef, pkgName, name)

    val resolvedImports =
      ReferenceResolve.resolveImports(imports, file): Iterable[(String, Importable, ImportStmt)]

    val pkgMap = HashMap[String, Pkg]()
    val clsMap = HashMap[String, Classlike]()

    //todo find a way to reduce code duplication
    pkgMap.addAll(rootPkg.subPkgs.view.map(p => p.name -> p))
    clsMap.addAll(currPkg.classlikes.map(c => c.name -> c))

    file.resolvedImports = resolvedImports.map { case (name, imported, imptStmt) =>
      imported match {
        case pkg: Pkg =>
          if (pkgMap.contains(name)) {
            Compiler.logError(
                s"Cannot import package ${name}: Pkg of same name already in scope",
                imptStmt,
                file
            )
          } else {
            pkgMap += name -> pkg
          }
        case cls: Classlike =>
          if (clsMap.contains(name)) {
            Compiler.logError(
                s"Cannot import class ${cls.name}: class of same name already in scope",
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

    file.classlikes.foreach(c => initialPassCls(c, clsIMap, pkgIMap, file))
  }

  //todo check modifiers and stuff
  /** Perform an initial pass over this class, resolving references and finding non-type-related errors.
    *
    * @param cls The current class
    * @param pkgMap A map of packages that are visible outside
    * @param clsMap A map of classes that are visible outside
    * @param file The current file
    */
  private def initialPassCls(
                              cls: Classlike,
                              clsRefs: Defs[Classlike],
                              pkgRefs: Defs[Pkg],
                              file: FileNode
  )(using logger: Logger): Unit = {
    val fieldRefs: Defs[VariableDecl] = cls.fields.view.map(f => f.name -> f).toMap

    cls match {
      case c: HasCtors if c.ctors.isEmpty => c.addCtor(Constructor.defaultCtor(c))
      case _                              =>
    }

    //This context is purely for ReferenceResolve.resolveType
    val dummyCtxt: Context = Context(Map.empty, Map.empty, Nil, Nil, clsRefs, pkgRefs, cls, file)

    cls.methods.foreach { mthd =>
      mthd match {
        case c: Constructor =>
          val mthdName = mthd.name
          if (mthdName != cls.name && mthdName != Keywords.constructorName) {
            Compiler.logError(s"Wrong constructor name: $mthdName", mthd, file)
          }
        case m: NormMethod =>
          m.returnType = m.returnType match {
            case tr: TypeRef =>
              ReferenceResolve.resolveType(tr)(using dummyCtxt)
            case primitive => primitive
          }
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
  )(using logger: Logger): Unit = {
    val isCtor = mthd.isInstanceOf[Constructor]
    given Context = Context(
        cls.fields.view.map(f => f.name -> f).toMap,
        cls.methods.groupBy(_.name).view.mapValues(mthds => MethodGroup(mthds.head.name, mthds)).toMap,
        cls.givenChildren,
        cls.proofChildren,
        clsRefs,
        pkgRefs,
        cls,
        file
    )
    mthd.body match {
      case Some(block) =>
        mthd.modifiers.find(_.modType == ModifierType.ABSTRACT) match {
          case Some(mod) =>
            Compiler.logError("Method with implementation cannot be abstract", mod)
          case None =>
            if (isCtor) {
              //TODO check if first statement is call to this() or super()
            }
        }
      case None =>
        if (!mthd.isAbstract)
          Compiler.logError("Method requires abstract modifier or implementation", mthd, file)
    }
  }

  def verifyPkgStmt(pkgRef: Option[PackageStmt], pkgName: String, fileName: String)(using
      logger: Logger
  ) =
    pkgRef match {
      case Some(pkgStmt) =>
        val foundText = pkgStmt.path.text
        if (foundText != pkgName)
          if (pkgName.nonEmpty)
            logger.error(
                s"Wrong package statement in file ${fileName}, should be $pkgName, found $foundText"
            )
      case None =>
        if (pkgName.nonEmpty)
          logger.error(s"No package statement in file ${fileName} in package $pkgName")
    }
}
