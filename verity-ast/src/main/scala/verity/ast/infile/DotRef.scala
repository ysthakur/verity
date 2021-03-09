package verity.ast.infile

import verity.parsing.TextRange

case class DotRef(
                   path: Iterable[Name]
) extends Node {
//  val startOffset: Int = iterable.head.startOffset
//  val endOffset: Int = iterable.last.endOffset
  override def text: String =
    path.map(_.text).mkString(".")
  //def this(pattern: PatternWithMatch[RepeatPattern[Node], Match[Node]]) = this(null)
}
