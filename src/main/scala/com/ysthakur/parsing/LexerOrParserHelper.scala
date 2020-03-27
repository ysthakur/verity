package com.ysthakur.parsing

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 *
 * @param lop The lexer or parser definition this is using
 * @tparam InputSource The type of the entire input (file/list of tokens)
 * @tparam Input       The type of a piece of input (char/token)
 * @tparam Output      The type of the result (list of tokens/AST)
 * @tparam Accumulator The type of what accumulates the input and stores it in
 *                     memory (StringBuilder/List of tokens)
 */
abstract class LexerOrParserHelper[InputSource, Input, Output, Accumulator <: Iterable[Input]]
(val lop: LexerOrParser[Input, Output, Accumulator]) {

    import lop._

    var current: Accumulator = emptyAccumulator()
    var currentState: State = defaultState
    private[parsing] var lastInput: Input = _
    /**
     * The last match it had. Includes the text that was matched
     */
    var lastMatch: (PatternCase[Input, Helper], Accumulator) = _
    private var running = false
    var offset: Int = 0

    def run(): Unit = {
        lastInput = getNext
        while (hasNext) {
            //if (current.nonEmpty) println(current)
            proceed(currentState)
            lastMatch._1.action(this.asInstanceOf[lop.Helper])
            update()
            println(lastMatch)
        }
    }

    def proceed(currentState: State): Unit = {
        accumulate(lastInput)
        val stateCase = stateCases.find(sc => sc.state == currentState).getOrElse(
            stateCases.find(sc => sc.state == defaultState)
                .getOrElse(throw new Error("No state matches this!"))
        )
        var possibleFutureMatches = mutable.Set[PatternCase[Input, Helper]]()
        var lastMatch: (PatternCase[Input, Helper], Accumulator) = null
        var (lastMatchLength, currentLength) = (0, 1)
        var currentPosition = offset
        do {
            breakable(for (pc <- stateCase.patternCases)
                pc.pattern.tryMatch(current.asInstanceOf[Iterable[Input]]) match {
                    case FullMatch(couldMatchMore) =>
                        if (lastMatchLength < currentLength) {
                            lastMatch = (pc, current.asInstanceOf[mutable.Cloneable[Accumulator]].clone())
                            lastMatchLength = currentLength
                        }
                        if (couldMatchMore) possibleFutureMatches += pc
                    case NeedsMore() => possibleFutureMatches.addOne(pc)
                    case NoMatch() | PartialMatch(_) => possibleFutureMatches -= pc
                })

            if (possibleFutureMatches.isEmpty) {
                val origOffset = offset
                offset = currentPosition
                if (lastMatch == null)
                    throw new Exception(s"""Bad character(s) "$current" at start offset $origOffset, ${getPosition}""")
                else {
                    this.lastMatch = lastMatch
                    lastInput = current.last
                    current = emptyAccumulator()
                    offset -= 1
                    println("Matched! Found=\"" + lastMatch._2 + "\" ending at offset=" + offset)
                    return
                }
            } else {
                if (!hasNext) throw new Exception("Unexpected end of file!")
                accumulate(current, getNext)
                currentPosition += 1
                currentLength += 1
            }
        } while (possibleFutureMatches.nonEmpty)
    }

    def process(): Output

    /**
     * Get the next character (if this is a lexer) or token (if this is a parser).
     *
     * @return The next piece of input or null if it's reached the end of the file
     *         or input stream
     */
    def getNext: Input

    /**
     * Whether or not it can proceed
     *
     * @return
     */
    def hasNext: Boolean

    def accumulate(acc: Accumulator, input: Input): Unit

    def accumulate(inputList: Iterable[Input]): Unit = inputList.foreach(accumulate)

    /**
     * Add this piece of the input (character/token) to the
     * accumulator, which is a StringBuilder or something.<br><br>
     * <em>Should not be called by any class other than this, but
     * it must be overridden</em>
     */
    def accumulate(input: Input): Unit = {
        accumulate(current, input)
        offset += 1
    }

    /**
     * Return the next character/token without consuming it.
     *
     * @return
     */
    def peekNext: Input

    /**
     * Return the last character/token that it processed
     *
     * @return
     */
    def peekLast: Input = lastInput

    def emptyAccumulator(): Accumulator

    protected def end(): Unit = {}

    def getPosition: String = offset.toString

    /**
     * Called after a token or node has been made
     */
    def update(): Unit = {}

}
