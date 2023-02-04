package verity.compiler.ast

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import cats.data.{Chain, NonEmptyChain}

type ModuleMember = ImportStmt | ModuleDef | TypeDef | VarDef

/** @param path
  *   The path of the import, excluding the wildcard
  * @param wildcard
  *   Whether everything inside the module is to be imported
  * @param qualified
  *   Whether it's a qualified import
  */
case class ImportStmt(
    path: NonEmptyChain[String],
    wildcard: Boolean = false,
    qualified: Boolean = false
) extends Tree {
  /** The module that the path resolved to */
  private[verity] var resolvedMod: ModuleDef | Null = null
  /** The type that the path resolved to */
  private[verity] var resolvedTypeDef: TypeDef | Null = null
  /** The global variable that the path resolved to */
  private[verity] var resolvedVar: GlobalVar | Null = null
}

/** A module definition
  *
  * Can't name it `Module` because of [[java.lang.Module]]
  */
sealed trait ModuleDef extends Tree {
  def name: String

  def submodules: Iterable[ModuleDef]
  def typeDefs: Iterable[TypeDef]
  def varDefs: Iterable[GlobalVar]
}

/** A module represented by a folder containing other modules
  *
  * @param name
  *   The module name (not absolute)
  * @param file
  *   The folder associated with the module
  */
case class FolderModule(
    override val name: String,
    folder: File,
    override val submodules: Seq[ModuleDef]
) extends ModuleDef {
  override def typeDefs = Nil
  override def varDefs = Nil
}

/** A module that has actual source code
  * @param name
  *   The module name (not absolute)
  * @param file
  *   If this module is an entire file, the real file it was in
  */
case class SourceModule(
    name: String,
    imports: Iterable[ImportStmt],
    submodules: Seq[ModuleDef],
    typeDefs: Seq[TypeDef],
    varDefs: Seq[GlobalVar],
    file: Option[File] = None
) extends ModuleDef

/** A common trait for modules loaded from already compiled code */
trait BinaryModule extends ModuleDef
