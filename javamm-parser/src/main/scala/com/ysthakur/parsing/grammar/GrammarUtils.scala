package com.ysthakur.parsing.grammar

object GrammarUtils {
  type -[A, B] = ConsPattern[A, B]
  type |[A, B] = OrPattern[A, B]
  type *[T] = RepeatPattern[T]
  type *?[T] = RepeatPattern[T]
}
