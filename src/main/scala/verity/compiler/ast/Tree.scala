package verity.compiler.ast

trait Tree

/** A definition of any sort that can be imported */
trait Def extends Tree {
  def name: String
}

/** Records the start and end of a piece of text in a file
  *
  * @param start
  *   Start offset of the text
  * @param end
  *   End offset of the text
  */
case class Span(start: Pos, end: Pos) {
  def isEmpty: Boolean = start.offset == end.offset

  /** Whether this Span didn't come from a real file */
  def isSynthetic: Boolean = start.isSynthetic || end.isSynthetic

  /** The length of the text */
  def length: Int = end.offset - start.offset

  override def equals(other: Any) = other match {
    case other: Span =>
      this.isSynthetic || other.isSynthetic || start == other.start && end == other.end
    case _ => false
  }
}

object Span {

  /** A Span that didn't come from a real file. Compares equal to any other Span
    */
  val synthetic = Span(Pos.synthetic, Pos.synthetic)

  def empty(pos: Pos) = Span(pos, pos)
}

/** Marks a row and column in a file */
case class Pos(offset: Int, row: Int, col: Int) {

  /** Whether this is a made-up position */
  def isSynthetic: Boolean = offset == -1

  /** Make a new position on the same row, but shifted right. Make sure the new
    * position is actually valid!
    */
  def +(shift: Int): Pos = Pos(offset + shift, row, col + shift)

  /** Make a new position on the same row, but shifted left. Make sure the new
    * position is actually valid!
    */
  def -(shift: Int): Pos =
    if (col >= shift) Pos(offset - shift, row, col + shift)
    else
      throw new IllegalArgumentException(
        s"Cannot shift left $shift characters from column $col"
      )
}

object Pos {
  val synthetic: Pos = Pos(-1, -1, -1)

  /** A position at the start of the file */
  val atStart: Pos = Pos(0, 0, 0)
}

case class Text(text: String, span: Span) {
  override def toString = text
}

object Text {
  def apply(text: String, span: Span = Span.synthetic) =
    new Text(text, span)
  def unapply(text: Text): Some[String] = Some(text.text)
}
