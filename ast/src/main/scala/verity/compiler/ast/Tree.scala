trait Tree

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
trait Def extends Tree {
  def name: String
}
