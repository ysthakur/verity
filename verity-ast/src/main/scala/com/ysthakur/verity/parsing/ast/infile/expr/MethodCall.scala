package com.ysthakur.verity.parsing.ast.infile.expr

import com.ysthakur.verity.parsing._
import com.ysthakur.verity.parsing.ast.infile._
import com.ysthakur.verity.parsing.TextRange

trait MethodCall() extends Expr {

}

case class ApplyCall(obj: Expr, args: ArgList, override val textRange: TextRange) extends MethodCall() {
  def text = obj.text + args.text
}

case class ArgList(args: List[Expr], textRange: TextRange) extends Node {
  def text = ???
}