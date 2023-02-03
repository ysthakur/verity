package verity.compiler.ast

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import cats.data.NonEmptyList

type ModuleMember = ImportStmt | ModuleDef | TypeDef | VarDef

/** @param path
  *   The path of the import, excluding the wildcard
  */
case class ImportStmt(path: NonEmptyList[String], wildcard: Boolean = false)
    extends Tree

/** A module definition
  *
  * Can't name it `Module` because of [[java.lang.Module]]
  */
sealed trait ModuleDef extends Tree {
  def name: String

  def submodules: Iterable[ModuleDef]
  def typeDefs: Iterable[TypeDef]
  def varDefs: Iterable[VarDef]

  /** Find a submodule given its relative path
    *
    * @return
    *   A `Right` if the module was found at the given path, otherwise a `Left`
    *   containing the part of the path at the end that wasn't found.
    */
  def findSubmodule(
      path: NonEmptyList[String]
  ): Either[NonEmptyList[String], ModuleDef] =
    this.submodules.find(_.name == path.head) match {
      case Some(subMod) =>
        path.tail match {
          case head :: tail => subMod.findSubmodule(NonEmptyList(head, tail))
          case _            => Right(subMod)
        }
      case None => Left(path)
    }
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
