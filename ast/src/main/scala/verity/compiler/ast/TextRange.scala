/** Records where a continuous piece of text is in a file
  *
  * @param start
  *   Start offset of the text
  * @param end
  *   End offset of the text
  */
case class TextRange(start: Int, end: Int) {
  def isEmpty: Boolean = start == end

  /** Whether this TextRange didn't come from a real file */
  def isSynthetic: Boolean =
    start == -1 && end == -1

  /** The length of the text */
  def length: Int = end - start

  override def equals(other: Any) = other match {
    case tr: TextRange =>
      this.isSynthetic || tr.isSynthetic || start == tr.start && end == tr.end
    case _ => false
  }
}

object TextRange {

  /** A TextRange that didn't come from a real file. Compares equal to any other
    * TextRange
    */
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
  def apply(text: String, textRange: TextRange = TextRange.synthetic) =
    new Text(text, textRange)
  def unapply(text: Text): Some[String] = Some(text.text)
}
