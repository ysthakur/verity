package verity.parsing

/**
 *
 * @param text - The text this token holds
 * @param textRange - Text range, start and end offset
 */
case class Token(text: String, textRange: TextRange) extends HasText

object Token {
  val hardKeywords = Array(
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

  val reservedWords = hardKeywords ++ Array(
    "where"
  )

  def empty(start: Int) = Token("", TextRange.empty(start))
}