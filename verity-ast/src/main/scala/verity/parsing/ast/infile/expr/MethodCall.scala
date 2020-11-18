package verity.parsing.ast.infile.expr

import verity.parsing._
import verity.parsing.ast.infile._

trait MethodCall() extends Expr {

}

case class ApplyCall(obj: Expr, valArgs: ArgList, override val textRange: TextRange) extends MethodCall() {
  def text = obj.text + valArgs.text
}

case class ArgList(args: List[Expr], textRange: TextRange) extends Node {
  def text = ???
}