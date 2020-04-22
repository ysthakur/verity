package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.HasText
import com.ysthakur.javamm.parsing.ast._
import com.ysthakur.javamm.parsing.ast.infile.Node
import com.ysthakur.javamm.parsing.lexer._

/**
  * An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(
    symbol: Token[SymbolTokenType]
) extends Node {
  def isBinary: Boolean = ???
  override def text: String = symbol.text/*
  override def startOffset: Int = symbol.startOffset
  override def endOffset: Int = symbol.endOffset*/
}
