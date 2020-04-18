package com.ysthakur.parsing.lexer

import com.ysthakur.parsing.HasText
import com.ysthakur.parsing.ast.Node
import com.ysthakur.parsing.ast.infile.TextNode

import scala.collection.mutable

type Tok = Token[TokenType]

/**
  *
  * @param tokenType The type of this token
  * @param startOffset The index in the file where this token starts
  * @param endOffset The index in the file <em>before</em> which this token ends
  */
sealed trait Token[+T <: TokenType] extends TextNode {
  def tokenType: T
  def text: String
  def pos: Position
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
    override val pos: Position
) extends Token[F] {
  override def text: String = tokenType.text
}

object InvariantToken {
  private val invariantTokenPool =
    mutable.Set[InvariantToken[FixedTextTokenType]]()
  def apply[F <: FixedTextTokenType](tt: F, pos: Position): InvariantToken[F] =
    invariantTokenPool
      .find(token => token.tokenType == tt)
      .getOrElse(new InvariantToken(tokenType = tt, pos))
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
    override val pos: Position
) extends Token[R]

object EmptyToken extends Token[TokenType] {
  override def pos: Position = ???
  override def text: String = ""
  override def tokenType: TokenType = ???
}