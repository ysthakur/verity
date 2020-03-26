package com.ysthakur.parsing.lexer

import java.util.regex.Pattern

import com.ysthakur.parsing
import com.ysthakur.parsing._

case class RegexPattern(regexStr: String) extends parsing.Pattern[Char] {
    override val isFixed: Boolean = false
    private val pattern = Pattern.compile(s"^$regexStr$$")
    override def tryMatch[T <: Iterable[Char]](input: T): MatchResult =
        RegexPattern.tryMatch(input, pattern)
}

object RegexPattern {
    @scala.annotation.tailrec
    def tryMatch[T <: Iterable[Char]](input: T, pattern: Pattern): MatchResult =
        input match {
            case inputStr: CharSequence =>
                val matcher = pattern.matcher(inputStr)
                if (matcher.matches()) FullMatch(matcher.requireEnd())
                else if (matcher.find(0)) PartialMatch(TextRange(matcher.start, matcher.end))
                else if (matcher.hitEnd()) NeedsMore()
                else NoMatch()
            case ls: List[Char] => tryMatch(ls.mkString : Iterable[Char], pattern)
            case _ => tryMatch(input.toList : Iterable[Char], pattern)
        }
}

case class FixedTextPattern(text: String) extends parsing.Pattern[Char] {
    /**
     * Whether or not it always matches the same input.
     * If false, it might be a valid identifier or something
     * that takes a variable length input or something like that
     */
    override val isFixed: Boolean = true
    private lazy val size = text.length

    override def tryMatch[T <: Iterable[Char]](inputChars: T): MatchResult = {
        val input = toStr(inputChars)
        val inLen = input.length
        if (input == text) return FullMatch(couldMatchMore = false)
        else if (inLen < size) {
            if (text.startsWith(input)) return NeedsMore()
        } else if (inLen > size) {
            if (input.indexOf(text) == 0) return PartialMatch(TextRange(0, size))
        }
        NoMatch()
    }
}