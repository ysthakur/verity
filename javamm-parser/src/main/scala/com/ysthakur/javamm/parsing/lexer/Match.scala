package com.ysthakur.javamm.parsing.lexer

/**
  * Merely wraps around a piece of input that was matched exactly.
  *
  * @param matched
  * @tparam Input
  */
case class Match(
    /*override val */matched: Iterable[Char],
    /*override val */start: Int,
    /*override val */end: Int
)
