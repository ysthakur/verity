package verity.parsing

case class TextRange(start: Int, end: Int) {
  def isEmpty: Boolean = start == end
  def length: Int = end - start
  def to(other: TextRange) = TextRange(this.start, other.end)
}

object TextRange {
  def empty(offset: Int) = TextRange(offset, offset)

  extension (start: Int)
    def to(end: Int) = TextRange(start, end)
  // def empty(posInt): TextRange = TextRange(pos, pos)
  //def toEnd(startInt)(toks: Iterable[Token[_]]): TextRange = TextRange(start, toks.last.pos)
}

// case class Position(var row: Int, var col: Int, var offset: Int) {
//   def copy()Int = Position(row, col, offset)
//   def in(tr: TextRange): Boolean = 
//     tr.start.offset < this.offset && this.offset < tr.end.offset
//   def to(otherInt): TextRange = TextRange(this, other)
// }