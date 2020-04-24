package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr
import com.ysthakur.javamm.parsing.{Token, ValidIdentifierTokenType}

case class ValidIdNode(token: Token[ValidIdentifierTokenType] /*, 
  override val startOffset: Int, 
  override val endOffset: Int*/) extends Expr {
  // def unapply(): (CharSequence, Int, Int) = ???
//  def startOffset: Int = ???
//  def endOffset: Int = ???
  override def text: String = token.text
}