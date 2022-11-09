package verity.ast

/**
  * Records where a continuous piece of text is in a file
  *
  * @param start Start offset of the text
  * @param end End offset of the text
  */
case class TextRange(start: Int, end: Int) {
  def isEmpty: Boolean = start == end
  def isSynthetic: Boolean = this == TextRange.synthetic
  def length: Int = end - start
  def to(other: TextRange) = TextRange(this.start, other.end)
}

object TextRange {
  val synthetic = TextRange(-1, -1)

  def empty(offset: Int) = TextRange(offset, offset)
}

/** Marks a row and column in a file */
case class Position(row: Int, col: Int)

object Position {
  def synthetic: Position = Position(-1, -1)
}

case class Text(text: String, textRange: TextRange) {
  override def toString = text
}

object Text {
  def apply(text: String, textRange: TextRange = TextRange.synthetic) = new Text(text, textRange)
  def unapply(text: Text): Some[String] = Some(text.text)
}
