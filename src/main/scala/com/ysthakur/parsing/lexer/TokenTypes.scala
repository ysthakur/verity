package com.ysthakur.parsing.lexer

import scala.collection.mutable
import scala.reflect.runtime.universe._

//TODO check all of the regexps
/*  */
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
    private[parsing] def allTokenTypes: mutable.Map[String, T]

    protected def apply(tokenTypeName: String): Option[T] = allTokenTypes.get(tokenTypeName)
}

sealed trait TokenTypesBase[TT <: TokenType] extends TokenTypeHolder[TT] {
    val ttype: Type

    def isInstance(obj: Any): Boolean

    override private[parsing] lazy val allTokenTypes: mutable.Map[String, TT] = {
        val rm = runtimeMirror(this.getClass.getClassLoader)
        val im = rm.reflect(this)
        val namesAndFields: Iterable[(String, TT)] = rm.classSymbol(this.getClass).toType.decls.filter {
            case getter: MethodSymbol => getter.isGetter && getter.isPublic
            case _ => false
        }.map { getter =>
            (getter.name.toString, im.reflectMethod(getter.asInstanceOf[MethodSymbol]).apply())
        }.filter {
            case (_, _: TokenType) => true
            case _ => false
        }.map(_.asInstanceOf[(String, TT)])
        mutable.LinkedHashMap.newBuilder.addAll(namesAndFields).result()
    }
}

object SymbolTokenTypes extends TokenTypesBase[FixedTextTokenType] {
    type FTT = FixedTextTokenType
    val LPAREN: FTT = make("(")
    val RPAREN: FTT = make(")")
    val LSQUARE: FTT = make("[")
    val RSQUARE: FTT = make("]")
    val LCURLY: FTT = make("{")
    val RCURLY: FTT = make("}")
    val COMMA: FTT = make(",")
    val SEMICOLON: FTT = make(";")
    val COLONX2: FTT = make("::")
    val COLON: FTT = make(":")
    val DOT: FTT = make(".")
    val LTEQ: FTT = make("<=")
    val GTEQ: FTT = make(">=")
    val EQX2: FTT = make("==")
    val LT: FTT = make("<")
    val GT: FTT = make(">")
    val NOTEQ: FTT = make("!=")
    val EQ: FTT = make("=")
    val ANDX2: FTT = make("&&")
    val ORX2: FTT = make("||")
    val AND: FTT = make("&")
    val OR: FTT = make("|")
    val CARET: FTT = make("^")
    val PLUS: FTT = make("+")
    val MINUS: FTT = make("-")
    val STAR: FTT = make("*")
    val FWDSLASH: FTT = make("/")
    val BACKSLASH: FTT = make("\\")
    val QUESTION: FTT = make("?")
    val MODULO: FTT = make("%")
    val AT: FTT = make("@")

    override val ttype: Type = typeOf[FixedTextTokenType]

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
        mutable.LinkedHashMap(reservedKeywords ++ usableKeywords: _*)
}

object VariantTextTokenTypes extends TokenTypesBase[RegexTokenType] {
    val WSP: RegexTokenType =
        new RegexTokenType("""\s+""") with IgnoredTokenType
    val VALID_ID: RegexTokenType with ValidIdentifierTokenType =
        new RegexTokenType("""[A-Za-z_$][A-Za-z0-9_$]*""") with ValidIdentifierTokenType
    val NUM_LITERAL: RegexTokenType = make("""-?[0-9]+(\.[0-9]+)?[FfDL]?""")
    val CHAR_LITERAL: RegexTokenType = make("""'([^\\']|\\.)'""")
    val STR_LITERAL: RegexTokenType = make(""""(\.|[^\\"])*"""")
    val SINGLE_LINE_COMMENT: RegexTokenType =
        new RegexTokenType("""//.*?(\r\n|\r|\n)""") with IgnoredTokenType
    val MULTILINE_COMMENT: RegexTokenType =
        new RegexTokenType("""/\*(.|\n)*?\*/""") with IgnoredTokenType

    override val ttype: Type = typeOf[RegexTokenType]

    protected def make(regex: String): RegexTokenType = RegexTokenType(regex)

    override def isInstance(obj: Any): Boolean = obj.isInstanceOf[RegexTokenType]
}

object JMMTokenTypes extends Dynamic {
    val fixedTextTokenTypes: mutable.Map[String, FixedTextTokenType] =
        mutable.LinkedHashMap((SymbolTokenTypes.allTokenTypes ++ KeywordTokenTypes.allTokenTypes).toSeq: _*)
    val allTokenTypes: mutable.Map[String, TokenType] =
        mutable.LinkedHashMap((fixedTextTokenTypes ++ VariantTextTokenTypes.allTokenTypes).toSeq: _*)

    def selectDynamic(tokenName: String): Option[TokenType] = allTokenTypes.get(tokenName)
}