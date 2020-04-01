package com.ysthakur.parsing.lexer

import scala.collection.mutable

//TODO check all of the regexps
/**
 *
 *
 * textMatters - Whether or not it has a different text each time
 *                    or all tokens of this type have the same text
 */
sealed trait TokenType(val textMatters: Boolean) {}
trait ValidIdentifierTokenType
trait FixedTextTokenType(val text: String) extends TokenType {
  override val textMatters: Boolean = false
}
trait IgnoredTokenType extends TokenType {}

enum SymbolTokenType(symbol: String) extends java.lang.Enum[SymbolTokenType] with FixedTextTokenType(symbol){
  case LPAREN extends SymbolTokenType("(")
  case RPAREN extends SymbolTokenType(")")
  case LSQUARE extends SymbolTokenType("[")
  case RSQUARE extends SymbolTokenType("]")
  case LCURLY extends SymbolTokenType("{")
  case RCURLY extends SymbolTokenType("}")
  case COMMA extends SymbolTokenType(",")
  case SEMICOLON extends SymbolTokenType(";")
  case COLONX2 extends SymbolTokenType("::")
  case COLON extends SymbolTokenType(":")
  case DOT extends SymbolTokenType(".")
  case LTEQ extends SymbolTokenType("<=")
  case GTEQ extends SymbolTokenType(">=")
  case EQX2 extends SymbolTokenType("==")
  case LT extends SymbolTokenType("<")
  case GT extends SymbolTokenType(">")
  case NOTEQ extends SymbolTokenType("!=")
  case EQ extends SymbolTokenType("=")
  case ANDX2 extends SymbolTokenType("&&")
  case ORX2 extends SymbolTokenType("||")
  case AND extends SymbolTokenType("&")
  case OR extends SymbolTokenType("|")
  case CARET extends SymbolTokenType("^")
  case PLUSX2 extends SymbolTokenType("++")
  case PLUS extends SymbolTokenType("+")
  case MINUSX2 extends SymbolTokenType("--")
  case MINUS extends SymbolTokenType("-")
  case STAR extends SymbolTokenType("*")
  case FWDSLASH extends SymbolTokenType("/")
  case BACKSLASH extends SymbolTokenType("\\")
  case QUESTION extends SymbolTokenType("?")
  case MODULO extends SymbolTokenType("%")
  case AT extends SymbolTokenType("@")
}

//TODO do this
enum KeywordTokenType(text: String) extends java.lang.Enum[KeywordTokenType] with FixedTextTokenType(text) {
  case IMPORT extends KeywordTokenType("import")
  case PACKAGE extends KeywordTokenType("package")
  case PUBLIC extends KeywordTokenType("public")
  case PRIVATE extends KeywordTokenType("private")
  case PROTECTED extends KeywordTokenType("protected")
  case SUPER extends KeywordTokenType("super")
  case DEFAULT extends KeywordTokenType("default")
  case EXTENDS extends KeywordTokenType("extends")
  case STATIC extends KeywordTokenType("static")
  case ABSTRACT extends KeywordTokenType("abstract")
  case FINAL extends KeywordTokenType("final")
  case NATIVE extends KeywordTokenType("native")
  case TRANSIENT extends KeywordTokenType("transient")
  case VOLATILE extends KeywordTokenType("volatile")
  case SYNCHRONIZED extends KeywordTokenType("synchronized")
  case CONST extends KeywordTokenType("const")
  case SWITCH extends KeywordTokenType("switch")
  case CASE extends KeywordTokenType("case")
  case WHILE extends KeywordTokenType("while")
  case FOR extends KeywordTokenType("for")
  case IF extends KeywordTokenType("if")
  case ELSE extends KeywordTokenType("else")
  case VAR extends KeywordTokenType("var")
  case CLASS extends KeywordTokenType("class")
  case TRAIT extends KeywordTokenType("trait")
  case ENUM extends KeywordTokenType("enum")
  case WHERE extends KeywordTokenType("where")
  case THROWS extends KeywordTokenType("throws")
  case IMPLIES extends KeywordTokenType("implies")
  case ASSERT extends KeywordTokenType("assert")
  case NEW extends KeywordTokenType("new")
  case CONTINUE extends KeywordTokenType("continue")
  case BREAK extends KeywordTokenType("break")
  case THROW extends KeywordTokenType("throw")
  case RETURN extends KeywordTokenType("return")
  case AS extends KeywordTokenType("as")
  case IS extends KeywordTokenType("is")
  case NULL extends KeywordTokenType("null")
  case TRUE extends KeywordTokenType("true")
  case FALSE extends KeywordTokenType("false")
  case WITH extends KeywordTokenType("with")
  case BOOL extends KeywordTokenType("bool")
  case BYTE extends KeywordTokenType("byte")
  case SHORT extends KeywordTokenType("short")
  case INT extends KeywordTokenType("int")
  case CHAR extends KeywordTokenType("char")
  case FLOAT extends KeywordTokenType("float")
  case DOUBLE extends KeywordTokenType("double")
  case LONG extends KeywordTokenType("long")
  case VOID extends KeywordTokenType("void")
}

enum RegexTokenType(val regex: String) extends java.lang.Enum[RegexTokenType] with TokenType(true) {
  case WSP extends RegexTokenType("""\s+""") with IgnoredTokenType
  case VALID_ID extends RegexTokenType("""[A-Za-z_$][A-Za-z0-9_$]*""") with ValidIdentifierTokenType
  case NUM_LITERAL extends RegexTokenType("""-?[0-9]+(\.[0-9]+)?[FfDL]?""")
  case CHAR_LITERAL extends RegexTokenType("""'([^\\']|\\.)'""")
  case STR_LITERAL extends RegexTokenType(""""(\.|[^\\"])*"""")
  case SINGLE_LINE_COMMENT extends RegexTokenType("""//.*?(\r\n|\r|\n)""") with IgnoredTokenType
  case MULTILINE_COMMENT extends RegexTokenType("""/\*(.|\n)*?\*/""") with IgnoredTokenType
}

object JMMTokenTypes {
  val allTokenTypes: Map[String, TokenType] = 
      (SymbolTokenType.values.asInstanceOf[Iterable[java.lang.Enum[_] with TokenType]] ++ 
        KeywordTokenType.values.asInstanceOf[Iterable[java.lang.Enum[_] with TokenType]] ++ 
        RegexTokenType.values.asInstanceOf[Iterable[java.lang.Enum[_] with TokenType]])
        .asInstanceOf[Iterable[java.lang.Enum[_] with TokenType]]
      .map[(String, TokenType)](x => (x.name, x))
      .toMap[String, TokenType]
}