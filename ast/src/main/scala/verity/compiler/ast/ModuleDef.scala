package verity.compiler.ast

import java.io.File
import scala.collection.mutable

import cats.data.NonEmptyList

type ModuleMember = ImportStmt | ModuleDef | TypeDef | VarDef

/** @param path
  *   The path of the import, excluding the wildcard
  */
case class ImportStmt(
  path: NonEmptyList[String],
  wildcard: Boolean = false
) extends Tree

/** A module definition
  *
  * Can't name it `Module` because of [[java.lang.Module]]
  */
case class ModuleDef(name: String, contents: Seq[ModuleMember]) {
  private val submodules = contents.collect { case m: ModuleDef => m }

  /** Find a submodule given its relative path
    *
    * @return
    *   A `Right` if the module was found at the given path, otherwise a `Left`
    *   containing the part of the path at the end that wasn't found.
    */
  def findSubmodule(
    path: NonEmptyList[String]
  ): Either[NonEmptyList[String], ModuleDef] =
    val NonEmptyList(subName, rest) = path

    this.submodules.find(_.name == subName) match {
      case Some(subMod) =>
        rest match {
          case head :: tail => subMod.findSubmodule(NonEmptyList(head, tail))
          case _            => Right(subMod)
        }
      case None => Left(path)
    }
}
