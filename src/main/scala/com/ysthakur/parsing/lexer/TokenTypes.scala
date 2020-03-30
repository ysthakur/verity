package com.ysthakur.parsing.lexer

import scala.collection.mutable
import scala.reflect.runtime.universe._

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
            case _                    => false
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
          case _                 => false
        }
        .map(_.asInstanceOf[(String, TT)])
    mutable.LinkedHashMap.newBuilder.addAll(namesAndFields).result()
  }
}

object SymbolTokenTypes extends TokenTypesBase[FixedTextTokenType] {
  type FTT = FixedTextTokenType with SymbolTokenType
  val LPAREN: FTT    = "("
  val RPAREN: FTT    = ")"
  val LSQUARE: FTT   = "["
  val RSQUARE: FTT   = "]"
  val LCURLY: FTT    = "{"
  val RCURLY: FTT    = "}"
  val COMMA: FTT     = ","
  val SEMICOLON: FTT = ";"
  val COLONX2: FTT   = "::"
  val COLON: FTT     = ":"
  val DOT: FTT       = "."
  val LTEQ: FTT      = "<="
  val GTEQ: FTT      = ">="
  val EQX2: FTT      = "=="
  val LT: FTT        = "<"
  val GT: FTT        = ">"
  val NOTEQ: FTT     = "!="
  val EQ: FTT        = "="
  val ANDX2: FTT     = "&&"
  val ORX2: FTT      = "||"
  val AND: FTT       = "&"
  val OR: FTT        = "|"
  val CARET: FTT     = "^"
  val PLUSX2: FTT    = "++"
  val PLUS: FTT      = "+"
  val MINUSX2: FTT   = "--"
  val MINUS: FTT     = "-"
  val STAR: FTT      = "*"
  val FWDSLASH: FTT  = "/"
  val BACKSLASH: FTT = "\\"
  val QUESTION: FTT  = "?"
  val MODULO: FTT    = "%"
  val AT: FTT        = "@"

  private implicit def m(
      text: String
  ): FixedTextTokenType with SymbolTokenType =
    new FixedTextTokenType(text) with SymbolTokenType
}

//TODO do this
object KeywordTokenTypes extends TokenTypesBase[KeywordTokenType] with Dynamic {
  type KTT = KeywordTokenType
  val IMPORT: KTT       = "import"
  val PACKAGE: KTT      = "package"
  val PUBLIC: KTT       = "public"
  val PRIVATE: KTT      = "private"
  val PROTECTED: KTT    = "protected"
  val SUPER: KTT        = "super"
  val DEFAULT: KTT      = "default"
  val EXTENDS: KTT      = "extends"
  val STATIC: KTT       = "static"
  val ABSTRACT: KTT     = "abstract"
  val FINAL: KTT        = "final"
  val NATIVE: KTT       = "native"
  val TRANSIENT: KTT    = "transient"
  val VOLATILE: KTT     = "volatile"
  val SYNCHRONIZED: KTT = "synchronized"
  val CONST: KTT        = "const"
  val SWITCH: KTT       = "switch"
  val CASE: KTT         = "case"
  val WHILE: KTT        = "while"
  val FOR: KTT          = "for"
  val IF: KTT           = "if"
  val ELSE: KTT         = "else"
  val VAR: KTT          = "var"
  val CLASS: KTT        = "class"
  val TRAIT: KTT        = "trait"
  val ENUM: KTT         = "enum"
  val WHERE: KTT        = "where"
  val THROWS: KTT       = "throws"
  val IMPLIES: KTT      = "implies"
  val ASSERT: KTT       = "assert"
  val NEW: KTT          = "new"
  val CONTINUE: KTT     = "continue"
  val BREAK: KTT        = "break"
  val THROW: KTT        = "throw"
  val RETURN: KTT       = "return"
  val AS: KTT           = "as"
  val IS: KTT           = "is"
  val NULL: KTT         = "null"
  val TRUE: KTT         = "true"
  val FALSE: KTT        = "false"
  val WITH: KTT         = "with"
  val BOOL: KTT         = "bool"
  val BYTE: KTT         = "byte"
  val SHORT: KTT        = "short"
  val INT: KTT          = "int"
  val CHAR: KTT         = "char"
  val FLOAT: KTT        = "float"
  val DOUBLE: KTT       = "double"
  val LONG: KTT         = "long"
  val VOID: KTT         = "void"

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
  val NUM_LITERAL: RegexTokenType  = make("""-?[0-9]+(\.[0-9]+)?[FfDL]?""")
  val CHAR_LITERAL: RegexTokenType = make("""'([^\\']|\\.)'""")
  val STR_LITERAL: RegexTokenType  = make(""""(\.|[^\\"])*"""")
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
