package verity.compiler.ast

import java.io.File
import scala.collection.mutable

import cats.data.NonEmptyList

/** A module definition
  *
  * Can't name it `Module` because of [[java.lang.Module]]
  */
case class ModuleDef(
  name: String,
  contents: Seq[ModuleMember]
)

type ModuleMember = ImportStmt | ModuleDef | TypeDef | VarDef

/** @param path
  *   The path of the import, excluding the wildcard
  */
case class ImportStmt(
  path: NonEmptyList[String],
  wildcard: Boolean = false
) extends Tree
