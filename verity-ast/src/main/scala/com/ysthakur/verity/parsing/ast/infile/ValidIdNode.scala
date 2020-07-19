package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.ast.infile.expr.Expr

case class ValidIdNode(name: String/*, 
  override val startOffset: Int, 
  override val endOffset: Int*/) extends Expr {
  // def unapply(): (CharSequence, Int, Int) = ???
//  def startOffset: Int = ???
//  def endOffset: Int = ???
  override def text: String = name
}