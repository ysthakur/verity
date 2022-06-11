package verity.ast

import collection.mutable.ArrayBuffer

trait Empty[T] {
  def empty: T
}

object Empty {
  def apply[T](using e: Empty[T]) = e.empty
}
