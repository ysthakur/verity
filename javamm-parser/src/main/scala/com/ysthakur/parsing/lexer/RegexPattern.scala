// package com.ysthakur.parsing.lexer

// import java.util.regex.Pattern

// import com.ysthakur.parsing.grammar
// import com.ysthakur.parsing.grammar.{ExactMatch, FullMatch, MatchResult, NeedsMore, NoMatch, PartialMatch, TextRange}

// case class RegexPattern(regexStr: String) extends grammar.Pattern[Char] {
//   override val isFixed: Boolean = false
//   private val pattern =
//     Pattern.compile(s"^$regexStr$$", Pattern.UNICODE_CHARACTER_CLASS)
//   override def tryMatch(input: Iterable[Char]): MatchResult =
//     try {
//       RegexPattern.tryMatch(input, pattern)
//     } catch {
//       case e: StackOverflowError =>
//         throw new Error(s"Input=$input, regex=$regexStr", e)
//     }
// }

// object RegexPattern {
//   @scala.annotation.tailrec
//   def tryMatch(input: Iterable[_ <: Char], pattern: Pattern): MatchResult =
//     input match {
//       case inputStr: CharSequence =>
//         val matcher = pattern.matcher(inputStr)
//         try {
//           if (matcher.matches())
//             FullMatch(
//                 ExactMatch(matcher.group(), TextRange(0, matcher.end)),
//                 matcher.requireEnd()
//             )
//           else if (matcher.find(0))
//             PartialMatch(ExactMatch(matcher.group(), 0, matcher.end))
//           else if (matcher.hitEnd()) NeedsMore()
//           else NoMatch()
//         } catch {
//           case e: StackOverflowError => throw e
//         }
//       case ls: List[Char] => tryMatch(ls.mkString: Iterable[Char], pattern)
//       case _              => tryMatch(input.toList: Iterable[Char], pattern)
//     }
// }
