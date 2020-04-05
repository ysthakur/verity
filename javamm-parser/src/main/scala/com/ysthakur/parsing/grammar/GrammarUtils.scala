package com.ysthakur.parsing.grammar

object GrammarUtils {
  type -[A <: Pattern[?], B <: Pattern[?]] = ConsPattern[A, B]
  type |[A <: Pattern[?], B <: Pattern[?]] = OrPattern[A, B]
  type *[T <: Pattern[?]] = RepeatPattern[T]
  type *?[T <: Pattern[?]] = RepeatPattern[T]
}
