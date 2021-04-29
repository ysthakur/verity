package verity.ast.infile

import verity.ast.*

case class DotRef(path: Iterable[(String, TextRange)]) extends Tree, HasText {
  override def textRange = TextRange(path.head._2.start, path.last._2.end)
  override def text = path.map(_._1).mkString(".")
}