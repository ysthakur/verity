package verity.parsing.ast.infile.expr

import verity.parsing._
import verity.parsing.ast.infile._
import verity.parsing.TextRange

trait MethodCall() extends Expr {

}

case class ApplyCall(obj: Expr, args: ArgList, override val textRange: TextRange) extends MethodCall() {
  def text = obj.text + args.text
}

case class ArgList(args: List[Expr], textRange: TextRange) extends Node {
  def text = ???
}