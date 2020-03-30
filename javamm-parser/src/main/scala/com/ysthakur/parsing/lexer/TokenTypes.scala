package com.ysthakur.parsing.lexer

import scala.collection.mutable
import scala.language.dynamics

//TODO check all of the regexps
/**
 *
 *
 * @param textMatters Whether or not it has a different text each time
 *                    or all tokens of this type have the same text
 */
sealed abstract class TokenType(val textMatters: Boolean)

/**
 * A type for a token that can be detected with some simple
 * regex
 *
 * @param regex The regex used to match input
 */
case class RegexTokenType(regex: String) extends TokenType(true)

case class FixedTextTokenType(text: String) extends TokenType(false)

trait ValidIdentifierTokenType

trait SymbolTokenType extends FixedTextTokenType {
  def unapply(): String = this.text
}

class KeywordTokenType(text: String) extends FixedTextTokenType(text)

trait IgnoredTokenType extends TokenType

/**
 * Represents something with a bunch of sub-token-types
 */
sealed trait TokenTypeHolder[T <: TokenType] {
  protected def apply(tokenTypeName: String): Option[T] =
    allTokenTypes.get(tokenTypeName)

  private[parsing] def allTokenTypes: mutable.Map[String, T]
}

sealed trait TokenTypesBase[TT <: TokenType] extends TokenTypeHolder[TT] {
  override private[parsing] lazy val allTokenTypes: mutable.Map[String, TT] = {
    val rm = runtimeMirror(this.getClass.getClassLoader)
    val im = rm.reflect(this)
    val namesAndFields: Iterable[(String, TT)] =
      rm.classSymbol(this.getClass)
          .toType
          .decls
          .filter { x =>
            x match {
              case getter: MethodSymbol => getter.isGetter && getter.isPublic
              case _ => false
            }
          }
          .map { getter =>
            (
                getter.name.toString,
                im.reflectMethod(getter.asInstanceOf[MethodSymbol]).apply()
            )
          }
          .filter {
            case (_, _: TokenType) => true
            case _ => false
          }
          .map(_.asInstanceOf[(String, TT)])
    mutable.LinkedHashMap.newBuilder.addAll(namesAndFields).result()
  }
}

enum SymbolTokenType(symbol: String) extends FixedTextTokenType {

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

  private implicit
  def m(text: String): FixedTextTokenType with SymbolTokenType =
    new FixedTextTokenType(text) with SymbolTokenType
}

//TODO do this
object KeywordTokenTypes extends TokenTypesBase[KeywordTokenType] {
  type KTT = KeywordTokenType
  case IMPORT
  extends KeywordTokenType("import")
  case PACKAGE
  extends KeywordTokenType("package")
  case PUBLIC
  extends KeywordTokenType("public")
  case PRIVATE
  extends KeywordTokenType("private")
  case PROTECTED
  extends KeywordTokenType("protected")
  case SUPER
  extends KeywordTokenType("super")
  case DEFAULT
  extends KeywordTokenType("default")
  case EXTENDS
  extends KeywordTokenType("extends")
  case STATIC
  extends KeywordTokenType("static")
  case ABSTRACT
  extends KeywordTokenType("abstract")
  case FINAL
  extends KeywordTokenType("final")
  case NATIVE
  extends KeywordTokenType("native")
  case TRANSIENT
  extends KeywordTokenType("transient")
  case VOLATILE
  extends KeywordTokenType("volatile")
  case SYNCHRONIZED
  extends KeywordTokenType("synchronized")
  case CONST
  extends KeywordTokenType("const")
  case SWITCH
  extends KeywordTokenType("switch")
  case CASE
  extends KeywordTokenType("case")
  case WHILE
  extends KeywordTokenType("while")
  case FOR
  extends KeywordTokenType("for")
  case IF
  extends KeywordTokenType("if")
  case ELSE
  extends KeywordTokenType("else")
  case VAR
  extends KeywordTokenType("var")
  case CLASS
  extends KeywordTokenType("class")
  case TRAIT
  extends KeywordTokenType("trait")
  case ENUM
  extends KeywordTokenType("enum")
  case WHERE
  extends KeywordTokenType("where")
  case THROWS
  extends KeywordTokenType("throws")
  case IMPLIES
  extends KeywordTokenType("implies")
  case ASSERT
  extends KeywordTokenType("assert")
  case NEW
  extends KeywordTokenType("new")
  case CONTINUE
  extends KeywordTokenType("continue")
  case BREAK
  extends KeywordTokenType("break")
  case THROW
  extends KeywordTokenType("throw")
  case RETURN
  extends KeywordTokenType("return")
  case AS
  extends KeywordTokenType("as")
  case IS
  extends KeywordTokenType("is")
  case NULL
  extends KeywordTokenType("null")
  case TRUE
  extends KeywordTokenType("true")
  case FALSE
  extends KeywordTokenType("false")
  case WITH
  extends KeywordTokenType("with")
  case BOOL
  extends KeywordTokenType("bool")
  case BYTE
  extends KeywordTokenType("byte")
  case SHORT
  extends KeywordTokenType("short")
  case INT
  extends KeywordTokenType("int")
  case CHAR
  extends KeywordTokenType("char")
  case FLOAT
  extends KeywordTokenType("float")
  case DOUBLE
  extends KeywordTokenType("double")
  case LONG
  extends KeywordTokenType("long")
  case VOID
  extends KeywordTokenType("void")

  implicit def make(keywordText: String): KTT =
    new KeywordTokenType(keywordText)

  def mwith(keywordText: String): KeywordTokenType =
    new KeywordTokenType(keywordText) with ValidIdentifierTokenType
}

object VariantTextTokenTypes extends TokenTypesBase[RegexTokenType] {
  val WSP: RegexTokenType =
    new RegexTokenType("""\s+""") with IgnoredTokenType
  val VALID_ID: RegexTokenType with ValidIdentifierTokenType =
    new RegexTokenType("""[A-Za-z_$][A-Za-z0-9_$]*""")
        with ValidIdentifierTokenType
  val NUM_LITERAL: RegexTokenType = make("""-?[0-9]+(\.[0-9]+)?[FfDL]?""")
  val CHAR_LITERAL: RegexTokenType = make("""'([^\\']|\\.)'""")
  val STR_LITERAL: RegexTokenType = make(""""(\.|[^\\"])*"""")
  val SINGLE_LINE_COMMENT: RegexTokenType =
    new RegexTokenType("""//.*?(\r\n|\r|\n)""") with IgnoredTokenType
  val MULTILINE_COMMENT: RegexTokenType =
    new RegexTokenType("""/\*(.|\n)*?\*/""") with IgnoredTokenType

  private def make(regex: String): RegexTokenType = RegexTokenType(regex)
}

object JMMTokenTypes extends Dynamic {
  val fixedTextTokenTypes: mutable.Map[String, FixedTextTokenType] =
    mutable.LinkedHashMap(
      (SymbolTokenTypes.allTokenTypes ++ KeywordTokenTypes.allTokenTypes).toSeq: _*
    )
  val allTokenTypes: mutable.Map[String, TokenType] =
    mutable.LinkedHashMap(
      (fixedTextTokenTypes ++ VariantTextTokenTypes.allTokenTypes).toSeq: _*
    )

  def selectDynamic(tokenName: String): Option[TokenType] =
    allTokenTypes.get(tokenName)
}
