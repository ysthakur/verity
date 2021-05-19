package verity.ast

import verity.ast.infile.Empty

case class TextRange(start: Int, end: Int) {
  def isEmpty: Boolean = start == end
  def isSynthetic: Boolean = this == TextRange.synthetic
  def length: Int = end - start
  def to(other: TextRange) = TextRange(this.start, other.end)
}

object TextRange {
  val synthetic = TextRange(-1, -1)

  def empty(offset: Int) = TextRange(offset, offset)
  // def empty(posInt): TextRange = TextRange(pos, pos)
  //def toEnd(startInt)(toks: Iterable[Token[_]]): TextRange = TextRange(start, toks.last.pos)
}

case class Text(text: String, textRange: TextRange) extends HasTextRange {
  override def toString = text
}
object Text {
  def apply(text: String, textRange: TextRange = TextRange.synthetic) = new Text(text, textRange)
  inline def unapply(text: Text): Some[String] = Some(text.text)
}

// case class Position(var row: Int, var col: Int, var offset: Int) {
//   def copy()Int = Position(row, col, offset)
//   def in(tr: TextRange): Boolean =
//     tr.start.offset < this.offset && this.offset < tr.end.offset
//   def to(otherInt): TextRange = TextRange(this, other)
// }
