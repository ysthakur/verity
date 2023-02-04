package verity.compiler.resolve

import verity.compiler.ast.BinaryModule
import verity.compiler.ast.FolderModule
import verity.compiler.ast.ImportStmt
import verity.compiler.ast.ModuleDef
import verity.compiler.ast.Path
import verity.compiler.ast.SourceModule
import verity.compiler.ast.Span
import verity.compiler.Context
import verity.compiler.Message
import verity.compiler.Result

import scala.collection.mutable

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.kernel.Semigroup
import cats.syntax.all.catsSyntaxSemigroup

case class ModuleGraph()

/** Analyze module dependency tree to see which modules have to be compiled
  * before which
  */
class ModuleDeps(root: ModuleDef) {
  def makeGraph(mods: Iterable[ModuleDef])(using Context): ModuleGraph = {
    val depsMap = makeDepsMap(mods)
    ???
  }

  /** Map modules to the paths of the modules that depend on them */
  private def makeDepsMap(
      mods: Iterable[ModuleDef],
  )(using Context): Result[Map[Path, Chain[Path]]] = {
    val depsMap = mutable.Map[Path, Chain[Path]]()

    def addDep(from: Path, to: Path): Unit =
      depsMap(from) =
        if (depsMap.contains(from)) depsMap(from) :+ to
        else Chain(to)

    def helper(mod: ModuleDef, currPath: Path): Chain[Message] = mod match {
      case sm: SourceModule =>
        // Get errors from resolving imports in this module
        val errors = sm.imports
          .flatMap { impt =>
            val error = resolveImport(mods, impt)
            if (impt.isMod) {
              addDep(currPath, impt.path)
            }
            if (impt.isTypeDef || impt.isVar) {
              // Depends on the module containing the imported type or var
              addDep(currPath, NonEmptyChain.fromChainUnsafe(impt.path.init))
            }
            error
          }

        val newPath = currPath :+ sm.name
        // Combine errors from this module with ones from submodules
        sm.submodules.view
          .map(helper(_, newPath))
          .foldLeft(Chain(errors*))(_ ++ _)
      case fm: FolderModule =>
        val newPath = currPath :+ fm.name
        fm.submodules.view.map(helper(_, newPath)).reduce(_ ++ _)
      case _: BinaryModule =>
        // Nothing to resolve inside an already compiled module
        Chain.nil
    }

    val errors =
      mods.view.map(mod => helper(mod, NonEmptyChain(mod.name))).reduce(_ ++ _)

    Result(depsMap.toMap, errors)
  }

  /** Resolve an import statement, returning a list of errors encountered, if
    * any
    * @param topMods
    *   Top-level modules
    * @param impt
    *   The import statement to resolve
    * @return
    *   An error message, if an error was found. None otherwise.
    */
  private def resolveImport(topMods: Iterable[ModuleDef], impt: ImportStmt)(
      using ctx: Context,
  ): Option[Message] = {
    topMods.find(_.name == impt.path.head) match {
      case Some(mod) =>
        NonEmptyChain.fromChain(impt.path.tail) match {
          case Some(remainingPath) =>
            resolveImportHelper(topMods, impt, mod, remainingPath)
          case None =>
            impt.isMod = true
            None
        }
      case None =>
        Some(
          Message.err(s"Module ${impt.path.head} not found", Span.synthetic),
        )
    }
  }

  /** Recursive helper for [[resolveImport]]
    * @param mod
    *   The module in which to search
    * @param remainingPath
    *   The part of the path left to search
    */
  @annotation.tailrec
  private def resolveImportHelper(
      topMods: Iterable[ModuleDef],
      impt: ImportStmt,
      mod: ModuleDef,
      remainingPath: Path,
  )(using Context): Option[Message] = {
    if (!impt.wildcard && remainingPath.tail.isEmpty) {
      // The import can only resolve to a typedef or variable if we're at the end
      impt.isTypeDef = mod.typeDefs.exists(_.name == remainingPath.head)
      impt.isVar = mod.varDefs.exists(_.name == remainingPath.head)
    }

    mod.submodules.find(_.name == remainingPath.head) match {
      case Some(subMod) =>
        NonEmptyChain.fromChain(remainingPath.tail) match {
          case Some(nextPath) =>
            resolveImportHelper(topMods, impt, subMod, nextPath)
          case None =>
            impt.isMod = true
            None
        }
      case None =>
        Some(
          Message.err(s"Module ${mod.name} has no member ", Span.synthetic),
        )
    }
  }
}
