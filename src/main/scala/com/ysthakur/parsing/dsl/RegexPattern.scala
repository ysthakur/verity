package com.ysthakur.parsing.dsl

import com.ysthakur.parsing.MatchResult

import scala.util.matching.Regex

case class RegexPattern(regexStr: String) extends Pattern[Char] {
    val regex = new Regex(regexStr)
    override def tryMatch[T <: Iterable[Input]](input: T):  = ???
}