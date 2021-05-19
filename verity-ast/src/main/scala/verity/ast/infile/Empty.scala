package verity.ast.infile

import verity.ast.TextRange

import collection.mutable.ArrayBuffer

trait Empty[T] {
  def empty: T
}

object Empty {
  def apply[T](using e: Empty[T]) = e.empty

  given Empty[TextRange] with
    def empty = TextRange.synthetic
}
