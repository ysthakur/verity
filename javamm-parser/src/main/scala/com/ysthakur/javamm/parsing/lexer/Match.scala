package com.ysthakur.javamm.parsing.lexer
//
//trait Match {
//  def start: Int
//  def end: Int
//  def matched: Iterable[Char]
//}

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
) //extends Match
//
//case class RegexMatch(
//    matched: Iterable[Char],
//    override val start: Int,
//    override val end: Int
//) extends Match
