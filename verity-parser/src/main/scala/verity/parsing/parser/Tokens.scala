package verity.parsing.parser

import verity.parsing.{Token, TokenType}

trait ValidIdentifierTokenType

enum SymbolToken(symbol: String) extends Pattern {
  case LPAREN extends SymbolToken("(")
  case RPAREN extends SymbolToken(")")
  case LSQUARE extends SymbolToken("[")
  case RSQUARE extends SymbolToken("]")
  case LCURLY extends SymbolToken("{")
  case RCURLY extends SymbolToken("}")
  case COMMA extends SymbolToken(",")
  case SEMICOLON extends SymbolToken(";")
  case COLONX2 extends SymbolToken("::")
  case COLON extends SymbolToken(":")
  case PIPELINE extends SymbolToken("|>")
  case RT_ARROW extends SymbolToken("->")
  case DOT extends SymbolToken(".")
  case LTX2 extends SymbolToken("<<")
  case GTX3 extends SymbolToken(">>>")
  case GTX2 extends SymbolToken("|>>")
  case LTEQ extends SymbolToken("<=")
  case GTEQ extends SymbolToken(">=")
  case EQX3 extends SymbolToken("===")
  case EQX2 extends SymbolToken("==")
  case LT extends SymbolToken("<")
  case GT extends SymbolToken(">")
  case NOTEQ extends SymbolToken("!=")
  case EQ extends SymbolToken("=")
  case ANDX2 extends SymbolToken("&&")
  case ORX2 extends SymbolToken("||")
  case AND extends SymbolToken("&")
  case OR extends SymbolToken("|")
  case CARET extends SymbolToken("^")
  case EXCL_MARK extends SymbolToken("!")
  case TILDE extends SymbolToken("~")
  case PLUSX2 extends SymbolToken("++")
  case PLUS extends SymbolToken("+")
  case MINUSX2 extends SymbolToken("--")
  case MINUS extends SymbolToken("-")
  case STAR extends SymbolToken("*")
  case FWDSLASH extends SymbolToken("/")
  case BACKSLASH extends SymbolToken("\\")
  case QUESTION extends SymbolToken("?")
  case MODULO extends SymbolToken("%")
  case AT extends SymbolToken("@")
  
  def apply(reader: Reader): ParseResult =
    val start = reader.offset
    reader.nextToken(symbol, TokenType.SYMBOL) match {
      case Some(token) => Matched(() => token, reader, token.textRange)
      case None => Failed(headOrEmpty(reader), List(symbol), start)
    }
}

enum ModifierToken(text: String) extends Pattern {
  case PUBLIC extends ModifierToken("public")
  case PRIVATE extends ModifierToken("private")
  case PROTECTED extends ModifierToken("protected")
  case THIS extends ModifierToken("this")
  case SUPER extends ModifierToken("super")
  case DEFAULT extends ModifierToken("default")
  case EXTENDS extends ModifierToken("extends")
  case STATIC extends ModifierToken("static")
  case ABSTRACT extends ModifierToken("abstract")
  case FINAL extends ModifierToken("final")
  case NATIVE extends ModifierToken("native")
  case TRANSIENT extends ModifierToken("transient")
  case VOLATILE extends ModifierToken("volatile")
  case SYNCHRONIZED extends ModifierToken("synchronized")
  case CONST extends ModifierToken("const")
  
  def apply(reader: Reader): ParseResult =
    val start = reader.offset
    reader.nextToken(text, TokenType.KEYWORD) match {
      case Some(token) => Matched(() => token, reader, token.textRange)
      case None => Failed(headOrEmpty(reader), List(text), start)
    }
}

enum KeywordToken(text: String) extends Pattern {
  case IMPORT extends KeywordToken("import")
  case PACKAGE extends KeywordToken("package")
  case PUBLIC extends KeywordToken("public")
  case PRIVATE extends KeywordToken("private")
  case PROTECTED extends KeywordToken("protected")
  case THIS extends KeywordToken("this")
  case SUPER extends KeywordToken("super")
  case DEFAULT extends KeywordToken("default")
  case EXTENDS extends KeywordToken("extends")
  case STATIC extends KeywordToken("static")
  case ABSTRACT extends KeywordToken("abstract")
  case FINAL extends KeywordToken("final")
  case NATIVE extends KeywordToken("native")
  case TRANSIENT extends KeywordToken("transient")
  case VOLATILE extends KeywordToken("volatile")
  case SYNCHRONIZED extends KeywordToken("synchronized")
  case CONST extends KeywordToken("const")
  case RULE extends KeywordToken("rule") with ValidIdentifierTokenType
  case SWITCH extends KeywordToken("switch")
  case CASE extends KeywordToken("case")
  case WHILE extends KeywordToken("while")
  case FOR extends KeywordToken("for")
  case IF extends KeywordToken("if")
  case ELSE extends KeywordToken("else")
  case CLASS extends KeywordToken("class")
  case INTERFACE extends KeywordToken("interface")
  case ENUM extends KeywordToken("enum")
  case EXTENSON extends KeywordToken("extension")
  case WHERE extends KeywordToken("where")
  case THROWS extends KeywordToken("throws")
  case IMPLIES extends KeywordToken("implies") with ValidIdentifierTokenType
  case ASSERT extends KeywordToken("assert")
  case NEW extends KeywordToken("new")
  case CONTINUE extends KeywordToken("continue")
  case BREAK extends KeywordToken("break")
  case THROW extends KeywordToken("throw")
  case RETURN extends KeywordToken("return")
  case AS extends KeywordToken("as")
  case INSTANCEOF extends KeywordToken("instanceof")
  case NULL extends KeywordToken("null")
  case TRUE extends KeywordToken("true")
  case FALSE extends KeywordToken("false")
  case BOOL extends KeywordToken("boolean")
  case BYTE extends KeywordToken("byte")
  case SHORT extends KeywordToken("short")
  case INT extends KeywordToken("int")
  case CHAR extends KeywordToken("char")
  case FLOAT extends KeywordToken("float")
  case DOUBLE extends KeywordToken("double")
  case LONG extends KeywordToken("long")
  case VOID extends KeywordToken("void")
  
  def apply(reader: Reader): ParseResult =
    val start = reader.offset
    reader.nextToken(text, TokenType.KEYWORD) match {
      case Some(token) => Matched(() => token, reader, token.textRange)
      case None => Failed(headOrEmpty(reader), List(text), start)
    }
}