package verity.parsing

import verity.ast.infile.Node

/**
 *
 * @param textRange - Text range, start and end offset
 * @param text - The text this token holds
 */
case class Token(textRange: TextRange, text: String, tokenType: TokenType = TokenType.MISC) {

}

enum TokenType {
  case DOC_COMMENT
  case MULTILINE_COMMENT
  case SINGLE_LINE_COMMENT
  case STRING
  case NUM_LITERAL
  case SYMBOL
  case KEYWORD
  case ALPHANUM
  case MISC
}

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

  def empty(start: Int) = Token(TextRange.empty(start), "")
}