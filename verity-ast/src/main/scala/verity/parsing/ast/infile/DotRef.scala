package verity.parsing.ast.infile

import verity.parsing.TextRange

case class DotRef(
                   iterable: Iterable[ValidId],
                   override val textRange: TextRange
) extends Node {
//  val startOffset: Int = iterable.head.startOffset
//  val endOffset: Int = iterable.last.endOffset
  override def text: String =
    iterable.map(_.text).mkString(".")
  //def this(pattern: PatternWithMatch[RepeatPattern[Node], Match[Node]]) = this(null)
}
