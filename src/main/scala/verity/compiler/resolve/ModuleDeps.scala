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
  )(using Context): Map[Path, Chain[Path]] = {
    val depsMap = mutable.Map[Path, Chain[Path]]()

    def helper(mod: ModuleDef, currPath: Path): Unit = mod match {
      case sm: SourceModule =>
        for (impt <- sm.imports) {
          resolveImport(mods, impt)
        }
      case fm: FolderModule =>
        val newPath = currPath :+ fm.name
        fm.submodules.foreach(helper(_, newPath))
      case _: BinaryModule =>
    }

    mods.foreach(mod => helper(mod, NonEmptyChain(mod.name)))

    depsMap.toMap
  }

  private def resolveImport(mods: Iterable[ModuleDef], impt: ImportStmt)(using
      ctx: Context,
  ): Chain[Message] = {
    @annotation.tailrec
    def helper(mod: ModuleDef, remainingPath: Path): Chain[Message] = {
      if (!impt.wildcard && remainingPath.tail.isEmpty) {
        // The import can only resolve to a typedef or variable if we're at the end
        mod.typeDefs.find(_.name == remainingPath.head).foreach { typeDef =>
          impt.resolvedTypeDef = typeDef
        }
        mod.varDefs.find(_.name == remainingPath.head).foreach { varDef =>
          impt.resolvedVar = varDef
        }
      }

      mod.submodules.find(_.name == remainingPath.head) match {
        case Some(subMod) =>
          NonEmptyChain.fromChain(remainingPath.tail) match {
            case Some(nextPath) => helper(subMod, nextPath)
            case None =>
              impt.resolvedMod = subMod
              Chain.nil
          }
        case None =>
          Chain(
            Message.err(s"Module ${mod.name} has no member ", Span.synthetic),
          )
      }
    }

    mods.find(_.name == impt.path.head) match {
      case Some(mod) =>
        NonEmptyChain.fromChain(impt.path.tail) match {
          case Some(remainingPath) => helper(mod, remainingPath)
          case None =>
            impt.resolvedMod = mod
            Chain.nil
        }
      case None =>
        Chain(
          Message.err(s"Module ${impt.path.head} not found", Span.synthetic),
        )
    }
  }
}
