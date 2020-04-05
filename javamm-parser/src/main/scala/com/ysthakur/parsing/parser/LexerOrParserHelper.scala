// package com.ysthakur.parsing.grammar

// import com.ysthakur.parsing.lexer.BadCharacterError

// import scala.collection.mutable
// import scala.util.control.Breaks.breakable

// /**
//   *
//   * @param lop The lexer or parser definition this is using
//   * @tparam InputSource The type of the entire input (file/list of tokens)
//   * @tparam Input       The type of a piece of input (char/token)
//   * @tparam Output      The type of the result (list of tokens/AST)
//   * @tparam Accumulator The type of what accumulates the input and stores it in
//   *                     memory (StringBuilder/List of tokens)
//   */
// abstract class LexerOrParserHelper[
//     InputSource,
//     Input,
//     Output,
//     Accumulator <: Iterable[Input]
// ](val lop: LexerOrParser[Input, Output, Accumulator]) {

//   import lop._

//   type A = Accumulator

//   var current: Accumulator = emptyAccumulator()
//   var currentState: String = firstState

//   /**
//     * The last match it had. Includes the [[com.ysthakur.parsing.grammar.PatternCase]] and the text
//     * that was matched and a tuple in the format (`startOffset`, `endOffset`).
//     * The end offset's not inclusive.
//     */
//   var lastMatch: (PatternCase[Input, Helper], Match[Input]) = _
//   var offset: Int                                           = 0
//   private[parsing] var lastInput: Input                     = _
//   private var running                                       = false

//   def run(): Unit = {
//     if (!hasNext) throw new Error("Empty input!!!")
//     while (lastInput.nonEmpty || {
//       if (hasNext) {
//         lastInput = List(getNext)
//         true
//       } else false
//     }) {
//       //if (current.nonEmpty) println(current)
//       proceed(currentState)
//       lastMatch._1.action(this.asInstanceOf[lop.Helper])
//       update()
//       println(lastMatch)
//     }
//   }

//   /*def proceed(currentState: String): Unit = {
//     accumulate(lastInput)
//     val stateCase = stateCases
//       .find(sc => sc._1 == currentState)
//       .getOrElse(
//           stateCases
//             .find(sc => sc._1 == firstState)
//             .getOrElse(throw new Error("No state matches this!"))
//       )
//     var possibleFutureMatches                                 = mutable.Set[PatternCase[Input, Helper]]()
//     var lastMatch: (PatternCase[Input, Helper], Match[Input]) = null
//     var (lastMatchLength, currentLength)                      = (0, 1)
//     var startOffset, currentOffset                            = offset
//     while ({
//       breakable(
//           for (pc <- stateCase._2)
//             pc.pattern.tryMatch(current.asInstanceOf[Iterable[Input]]) match {
//               case FullMatch(matched, couldMatchMore) =>
//                 if (lastMatchLength < currentLength) {
//                   lastMatch = (pc, matched.asInstanceOf[Match[Input]])
//                   lastMatchLength = currentLength
//                 }
//                 if (couldMatchMore) possibleFutureMatches += pc
//               case NeedsMore()                 => possibleFutureMatches.addOne(pc)
//               case NoMatch() | PartialMatch(_) => possibleFutureMatches -= pc
//             }
//       )

//       if (possibleFutureMatches.isEmpty) {
//         if (lastMatch == null)
//           throw new Exception(
//               s"""Bad character(s) "$current" at start offset $startOffset, ${getPosition}"""
//           )
//         else {
//           this.lastMatch = (lastMatch._1, lastMatch._2)
//           //TODO fix this so that when it has to be rolled back because it went more than
//           //  one character too far, it doesn't fail. For now, it seems to work
//           //  with the current grammar
//           val last = current.last
//           this.lastInput = if (this.lastInput == last) getNext else last
//           current = emptyAccumulator()
//           offset = lastMatch._2.end
//           println(
//               "Matched! Found=\"" + lastMatch._2 + "\" ending at offset=" + offset
//           )
//           return
//         }
//       } else {
//         if (!hasNext) throw new Exception("Unexpected end of file!")
//         accumulate(current, getNext)
//         currentOffset += 1
//         currentLength += 1
//       }
//       possibleFutureMatches.nonEmpty
//     }) {}
//   }*/

//   def proceed(state: String): Unit = {
//     for (stateCase <- stateCases) {
//       val state = stateCase._1
//       if (state == currentState || state == catchAllState) {
//         val res = tryMatch(stateCase._2, offset).getOrElse()
//       }
//     }
//     throw new Error("Input does not match any pattern")
//   }

//   def tryMatch(
//       patternCases: Iterable[_PatternCase_],
//       startOffset: Int
//   ): Option[(_PatternCase_, _Match_, _Iterable_)] = {
//     val acc = emptyAccumulator()
//     var possibleFutureMatches = mutable.Set[PatternCase[Input, Helper]]()
//     var lastMatch: (PatternCase[Input, Helper], Match[Input]) = null
//     var (lastMatchLength, currentLength) = (0, 1)
//     var startOffset, currentOffset = offset
//     val current = 0
//     while ({
//       breakable(
//           for (pc <- patternCases)
//             pc.pattern.tryMatch(acc.asInstanceOf[_Iterable_], currentOffset) match {
//               case FullMatch(matched, couldMatchMore) => {
//                 if (lastMatchLength < currentLength) {
//                   lastMatch = (pc, matched.asInstanceOf[_Match_])
//                   lastMatchLength = currentLength
//                 }
//                 if (couldMatchMore) possibleFutureMatches += pc
//               }
//               case NeedsMore()                 => possibleFutureMatches.addOne(pc)
//               case NoMatch() | PartialMatch(_) => possibleFutureMatches -= pc
//             }
//       )

//       if (possibleFutureMatches.isEmpty) {
//         if (lastMatch == null)
//           throw new Exception(
//               s"""Bad character(s) "$acc" at start offset $startOffset, ${getPosition}"""
//           )
//         else {
//           this.lastMatch = (lastMatch._1, lastMatch._2)
//           println(
//               "Matched! Found=\"" + lastMatch._2 + "\""
//           )
//           return Some((lastMatch._1, lastMatch._2, acc.slice(lastMatch._2.end, acc.size)))
//         }
//       } else {
//         if (!hasNext) throw new Exception("Unexpected end of file!")
//         accumulate(acc, getNext)
//         currentOffset += 1
//         currentLength += 1
//       }
//       possibleFutureMatches.nonEmpty
//     }) {}
//     None
//   }

//   /**
//     * The string representation of this lexer or parser's current
//     * position.
//     *
//     * @return
//     */
//   def getPosition: String = offset.toString

//   /**
//     * Called after a token or node has been made
//     */
//   def update(): Unit = {}

//   def process(): Output

//   /**
//     * Get the next character (if this is a lexer) or token (if this is a parser).
//     *
//     * @return The next piece of input or null if it's reached the end of the file
//     *         or input stream
//     */
//   def getNext: Input

//   /**
//     * Whether or not it can proceed
//     *
//     * @return
//     */
//   def hasNext: Boolean

//   def accumulate(acc: Accumulator, input: Input): Unit

//   def accumulate(inputList: Iterable[Input]): Unit =
//     inputList.foreach(accumulate)

//   /**
//     * Add this piece of the input (character/token) to the
//     * accumulator, which is a StringBuilder or something.<br><br>
//     * <em>Should not be called by any class other than this, but
//     * it must be overridden</em>
//     */
//   def accumulate(input: Input): Unit = {
//     accumulate(current, input)
//     offset += 1
//   }

//   /**
//     * Return the next character/token without consuming it.
//     *
//     * @return
//     */
//   def peekNext: Input

//   /**
//     * Return the last character/token that it processed
//     *
//     * @return
//     */
//   def peekLast: Input = lastInput

//   def emptyAccumulator(): Accumulator

//   protected def end(): Unit = {}

// }
