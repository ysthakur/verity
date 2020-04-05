// package com.ysthakur.parsing.grammar

// import com.ysthakur.parsing.lexer.RegexPattern

// class State(name: String) {
//   def :=[Input, Helper]
//       (patternCases: PatternCase[Input, Helper]*)
//       (implicit tokenizer: LexerOrParser[_, _, _]): Unit =
//           tokenizer.addState(
//               name,
//               patternCases.asInstanceOf[Iterable[
//                   PatternCase[tokenizer.I, tokenizer.Helper]
//               ]]
//           )
// }

// implicit def toState(name: String): State = new State(name)

// implicit def toRegex(regex: String): RegexPattern = RegexPattern(regex)

// def toStr(chars: Iterable[Char]): String = {
//     chars match {
//         case str: scala.collection.immutable.WrappedString => str.toString
//         case charseq: CharSequence => charseq.toString
//         case _ => chars.foldRight(new StringBuilder())((c, b) => b.append(c)).toString
//     }
// }