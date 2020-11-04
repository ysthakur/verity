package verity.parsing.ast.infile.expr

import verity.parsing._
import verity.parsing.ast.infile.ValidIdNode

case class DotRef(iterable: Iterable[ValidIdNode], override val textRange: TextRange) extends Expr {
//  val startOffset: Int = iterable.head.startOffset
//  val endOffset: Int = iterable.last.endOffset
  override def text: String =
    iterable.map(_.text).mkString(".")
  //def this(pattern: PatternWithMatch[RepeatPattern[Node], Match[Node]]) = this(null)
}