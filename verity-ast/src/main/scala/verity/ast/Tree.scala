package verity.ast

trait Tree {
  def synthetic: Boolean = false
}

object Tree {
  val hardKeywords: List[String] = List(
    "if",
    "else",
    "true",
    "false",
    "import",
    "package",
    "void",
    "boolean",
    "char",
    "number",
    "_"
  )
}

/** A definition of any sort that can be imported */
trait Def extends Tree

trait Synthetic extends Tree

trait ParentNode extends Tree {
  type Child <: Tree
  def children: Iterable[Child]
}

trait ChildNode extends Tree {
  type Parent <: ParentNode
  def parent: Parent
}

trait NamedTree extends Tree {
  def name: String
}
