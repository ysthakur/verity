package verity.ast

trait Tree {
  def synthetic: Boolean = false
}

object Tree {
  val hardKeywords: List[String] = List(
    "class",
    "interface",
    "enum",
    "object",
    "extension",
    "final",
    "const",
    "goto",
    "public",
    "protected",
    "private",
    "synchronized",
    "transient",
    "volatile",
    "native",
    "if",
    "else",
    "for",
    "while",
    "do",
    "switch",
    "case",
    "default",
    "break",
    "continue",
    "throw",
    "return",
    "new",
    "instanceof",
    "null",
    "true",
    "false",
    "this",
    "super",
    "import",
    "package",
    "void",
    "boolean",
    "byte",
    "short",
    "char",
    "int",
    "long",
    "float",
    "double",
    "_"
  )
}

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
