package verity.ast.infile

import verity.ast.infile.Expr
import verity.parsing.TextRange

import scala.collection.mutable.ListBuffer

case class Block(stmts: ListBuffer[Statement], override val textRange: TextRange) extends Expr {
  def text = stmts.map(_.text).mkString("{", "", "}")
}