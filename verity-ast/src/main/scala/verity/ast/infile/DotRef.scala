package verity.ast.infile

import verity.ast.Tree
import verity.parsing.{TextRange, HasText}

case class DotRef(path: Iterable[Name]) extends Tree, HasText {
  override def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
  override def text = path.map(_.text).mkString(".")
}