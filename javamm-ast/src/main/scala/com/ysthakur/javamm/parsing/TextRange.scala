package com.ysthakur.javamm.parsing

case class TextRange(start: Position, end: Position) {
  def isEmpty: Boolean = start == end
  def length: Int = end.offset - start.offset
}

object TextRange {
  def empty(pos: Position): TextRange = TextRange(pos, pos)
  //def toEnd(start: Position)(toks: Iterable[Token[_]]): TextRange = TextRange(start, toks.last.pos)
}

case class Position(var row: Int, var col: Int, var offset: Int) {
  def copy(): Position = Position(row, col, offset)
  def in(tr: TextRange): Boolean = 
    tr.start.offset < this.offset && this.offset < tr.end.offset
  def to(other: Position): TextRange = TextRange(this, other)
}