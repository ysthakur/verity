package com.ysthakur.parsing.lexer

import scala.collection.mutable
import scala.util.matching.Regex

sealed trait TokenType

/**
 * A type for a token that can be detected with some simple
 * regex
 * @param regex
 */
case class RegexTokenType(regex: Regex) extends TokenType {
    def apply(regex: String): RegexTokenType = RegexTokenType(new Regex(regex))
}

case object RegexTokenTypes extends TokenType {
    type This = RegexTokenType

    val WSP: This = make("\\W+")
    val KEYWORD: This = make("")
    val VALID_ID: This = make("")
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
    val AT: This = make("@")

    def make(regex: String): RegexTokenType = {
        RegexTokenType(new Regex(regex))
    }

    private val allTokenTypes = this.getClass.getDeclaredFields.filter { field =>
        field.get(this) match {
            case RegexTokenType(_) => true
            case _ => false
        }
    }.map(field => (field.getName, field.get(this).asInstanceOf)).toMap[String, RegexTokenType]

    def apply(tokenTypeName: String): Option[RegexTokenType] = allTokenTypes.get(tokenTypeName)
}

object ComplexTokenTypes extends Enumeration {
    val NUM_LITERAL, CHAR_LITERAL, STR_LITERAL = new TokenType(){}
}

/*
object TokenTypes extends Enumeration with TokenType {
    val
    WSP,
    KEYWORD,
    VALID_ID,
    LPAREN, RPAREN, LSQUARE, RSQUARE, LCURLY, RCURLY,
    COMMA, SEMICOLON, COLONX2, COLON, DOT, EQ,
    LTEQ, GTEQ, EQX2, LT, GT, NOTEQ,
    ANDX2, ORX2, AND, OR, CARET,
    PLUS, MINUS, STAR, FWDSLASH, BACKSLASH, QUESTION, MODULO,
    NUM_LITERAL,
    CHAR_LITERAL, STR_LITERAL,
    AT
    = Value

    val simpleRegexTokenTypes = List(WSP, KEYWORD)
}*/
