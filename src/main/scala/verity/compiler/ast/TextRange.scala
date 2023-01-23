package verity.compiler.ast

/** Records where a continuous piece of text is in a file
  *
  * @param start
  *   Start offset of the text
  * @param end
  *   End offset of the text
  */
case class Span(start: Int, end: Int) {
  def isEmpty: Boolean = start == end

  /** Whether this Span didn't come from a real file */
  def isSynthetic: Boolean =
    start == -1 && end == -1

  /** The length of the text */
  def length: Int = end - start

  override def equals(other: Any) = other match {
    case span: Span =>
      this.isSynthetic || span.isSynthetic || start == span.start && end == span.end
    case _ => false
  }
}

object Span {

  /** A Span that didn't come from a real file. Compares equal to any other
    * Span
    */
  val synthetic = Span(-1, -1)

  def empty(offset: Int) = Span(offset, offset)
}

/** Marks a row and column in a file */
case class Position(row: Int, col: Int)

object Position {
  def synthetic: Position = Position(-1, -1)
}

case class Text(text: String, span: Span) {
  override def toString = text
}

object Text {
  def apply(text: String, span: Span = Span.synthetic) =
    new Text(text, span)
  def unapply(text: Text): Some[String] = Some(text.text)
}
