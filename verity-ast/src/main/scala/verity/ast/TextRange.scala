package verity.ast

case class TextRange(start: Position, end: Position) {
  def isEmpty: Boolean = start == end
  def isSynthetic: Boolean = this == TextRange.synthetic
  def length: Int = ??? //end - start
  def to(other: TextRange) = TextRange(this.start, other.end)
}

object TextRange {
  val synthetic = TextRange(Position.synthetic, Position.synthetic)

  def empty(offset: Position) = TextRange(offset, offset)
  // def empty(posInt): TextRange = TextRange(pos, pos)
  //def toEnd(startInt)(toks: Iterable[Token[_]]): TextRange = TextRange(start, toks.last.pos)
}

case class Position(row: Int, col: Int, offset: Int) {
  // def in(tr: TextRange): Boolean =
  //   tr.start.offset < this.offset && this.offset < tr.end.offset
  def to(other: Position): TextRange = TextRange(this, other)

  override def toString = s"$row:$col"
}

object Position {
  val synthetic = Position(-1, -1, -1)
}

case class Text(text: String, textRange: TextRange) extends HasTextRange {
  override def toString = text
}
object Text {
  def apply(text: String, textRange: TextRange = TextRange.synthetic) = new Text(text, textRange)
  inline def unapply(text: Text): Some[String] = Some(text.text)
}
