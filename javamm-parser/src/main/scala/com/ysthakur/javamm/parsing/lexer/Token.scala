package com.ysthakur.javamm.parsing.lexer

import com.ysthakur.javamm.parsing.{HasText, Position, TextRange}
import com.ysthakur.javamm.parsing.ast.INode
import com.ysthakur.javamm.parsing.ast.infile.Node

import scala.collection.mutable

type Tok = Token[TokenType]

/**
  *
  * @param tokenType The type of this token
  * @param startOffset The index in the file where this token starts
  * @param endOffset The index in the file <em>before</em> which this token ends
  */
sealed trait Token[+T <: TokenType] extends Node {
  def tokenType: T
  def text: String
  def range: TextRange
}

object Token {
  def isValidId(token: Token[?]): Boolean =
    token.tokenType.isInstanceOf[ValidIdentifierTokenType]
  def unapply[T <: TokenType](arg: Token[T]): Option[T] = Some(arg.tokenType)
  /*def unapply[T <: TokenType](arg: Token[T]): Option[(T, Int, Int)] =
    Some(arg.tokenType, arg.startOffset, arg.endOffset)*/
}

/**
  * A token whose text is always the same
  *
  * @param tokenType
  */
case class InvariantToken[+F <: FixedTextTokenType] private (
    override val tokenType: F,
    override val range: TextRange
) extends Token[F] {
  override def text: String = tokenType.text
}

object InvariantToken {
  private val invariantTokenPool =
    mutable.Set[InvariantToken[FixedTextTokenType]]()
  def apply[F <: FixedTextTokenType](tt: F, range: TextRange): InvariantToken[F] =
    invariantTokenPool
      .find(token => token.tokenType == tt)
      .getOrElse(new InvariantToken(tokenType = tt, range))
      .asInstanceOf[InvariantToken[F]]
}

/**
  * A token whose text may be different, like a string literal
  *
  * @param tokenType
  */
case class VariantToken[+R <: RegexTokenType](
    override val tokenType: R,
    override val text: String,
    override val range: TextRange
) extends Token[R]

//TODO remove this or replace it with a class
object EmptyToken extends Token[TokenType] {
  override def range: TextRange = ???
  override def text: String = ""
  override def tokenType: TokenType = ???
}