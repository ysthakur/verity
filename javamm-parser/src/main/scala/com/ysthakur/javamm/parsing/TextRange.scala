package com.ysthakur.javamm.parsing

case class TextRange(start: Position, end: Position) {
  def isEmpty: Boolean = start == end
  def length: Int = end.offset - start.offset
}

object TextRange {
  def empty(pos: Position): TextRange = TextRange(pos, pos)
}

case class Position(var row: Int, var col: Int, var offset: Int) {
  def copy(): Position = Position(row, col, offset)
}