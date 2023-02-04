package verity.compiler

import verity.compiler.ast.ModuleDef

/** Represents an entire project
  * @param mods
  *   The topmost modules in the project
  */
case class Project(mods: Iterable[ModuleDef])
