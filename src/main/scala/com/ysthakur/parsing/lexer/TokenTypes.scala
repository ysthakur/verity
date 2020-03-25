package com.ysthakur.parsing.lexer

import scala.collection.mutable

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

trait SymbolTokenType extends FixedTextTokenType

class KeywordTokenType(text: String) extends FixedTextTokenType(text)

trait IgnoredTokenType extends TokenType

/**
 * Represents something with a bunch of sub-token-types
 */
sealed trait TokenTypeHolder[T <: TokenType] {
    protected def allTokenTypes: mutable.Map[String, T]

    protected def apply(tokenTypeName: String): Option[T] = allTokenTypes.get(tokenTypeName)
}

trait TokenTypesBase[TT <: TokenType] extends TokenTypeHolder[TT] {
    def isInstance(obj: Any): Boolean
    override lazy val allTokenTypes: mutable.Map[String, TT] =
        mutable.LinkedHashMap(this.getClass.getDeclaredFields
            .filter { field => field.canAccess(this) && isInstance(field.get(this)) }
            .map { field => ((field.getName) -> field.get(this).asInstanceOf[TT]) }: _*)
}

object SymbolTokenTypes extends TokenTypesBase[FixedTextTokenType] {
    var LPAREN, RPAREN, LSQUARE, RSQUARE, LCURLY, RCURLY =
        (make("("), make(")"), make("["),
            make("]"), make("{"), make("}"))
    val COMMA, SEMICOLON, COLONX2, COLON, DOT =
        (make(","), make(";"), make("::"), make(":"), make("."))
    val LTEQ, GTEQ, EQX2, LT, GT, NOTEQ, EQ =
        (make("<="), make(">="), make("=="),
            make("<"), make(">"), make("!="), make("="))
    val ANDX2, ORX2, AND, OR, CARET =
        (make("&&"), make("||"), make("&"),
            make("|"), make("^"))
    val PLUS, MINUS, STAR, FWDSLASH, BACKSLASH, QUESTION, MODULO =
        (make("+"), make("-"), make("*"), make("/"), make("\\"),
            make("?"), make("%"))
    val AT: FixedTextTokenType = make("@")

    protected def make(text: String): FixedTextTokenType = FixedTextTokenType(text)
    override def isInstance(obj: Any): Boolean = obj.isInstanceOf[SymbolTokenType]
}

//TODO do this
object KeywordTokenTypes extends TokenTypeHolder[KeywordTokenType] {
    /**
     * Keywords that cannot be used as valid identifiers
     */
    private val reservedKeywords = List(
        "import", "package", "public", "private", "protected", "super", "default", "extends",
        "static", "abstract", "final", "native", "transient", "volatile", "synchronized", "const",
        "switch", "case", "while", "for", "if", "else", "var", "class", "trait", "enum",
        "where", "throws", "implies", "assert", "new", "continue", "break", "throw", "return",
        "as", "is", "null", "true", "false", "with",
        "bool", "byte", "short", "int", "char", "float", "double", "long", "void")
        .map(word => (word.toUpperCase, new KeywordTokenType(word)))
    /**
     * Keywords that can be used as variable names and stuff in a certain context
     */
    private val usableKeywords = List("sealed", "annotation", "impl", "rule")
        .map(word => (word.toUpperCase, new KeywordTokenType(word) with ValidIdentifierTokenType))

    override val allTokenTypes: mutable.Map[String, KeywordTokenType] =
        mutable.LinkedHashMap((reservedKeywords ++ usableKeywords): _*)
}

object VariantTextTokenTypes extends TokenTypesBase[RegexTokenType] {
    val WSP: RegexTokenType =
        new RegexTokenType("\\W+") with IgnoredTokenType
    val VALID_ID: ValidIdentifierTokenType =
        new RegexTokenType("""[A-Za-z_$][A-Za-z0-9_$]*""") with ValidIdentifierTokenType
    val NUM_LITERAL: RegexTokenType = make("""-?[0-9]+(\.[0-9]+)?[FfDL]?""")
    val CHAR_LITERAL: RegexTokenType = make("""'([^\\']|\\.)'""")
    val STR_LITERAL: RegexTokenType = make(""""(\.|[^\\"])*"""")
    val SINGLE_LINE_COMMENT: RegexTokenType =
        new RegexTokenType("""//.*?\n""") with IgnoredTokenType
    val MULTILINE_COMMENT: RegexTokenType =
        new RegexTokenType("/\\*[^]*?\\*/") with IgnoredTokenType

    protected def make(regex: String): RegexTokenType = RegexTokenType(regex)
    override def isInstance(obj: Any): Boolean = obj.isInstanceOf[RegexTokenType]
}

object JMMTokenTypes extends Dynamic {
    val fixedTextTokenTypes: mutable.Map[String, FixedTextTokenType] =
        mutable.LinkedHashMap((SymbolTokenTypes.allTokenTypes ++ KeywordTokenTypes.allTokenTypes).toSeq: _*)
    val allTokenTypes: mutable.Map[String, TokenType] = mutable.LinkedHashMap((fixedTextTokenTypes ++ VariantTextTokenTypes.allTokenTypes).toSeq: _*)

    def selectDynamic(tokenName: String): Option[TokenType] = allTokenTypes.get(tokenName)
}