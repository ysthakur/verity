package com.ysthakur.verity.parsing.lexer

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class Match(
    matched: Iterable[Char],
    start: Int,
    end: Int
)
