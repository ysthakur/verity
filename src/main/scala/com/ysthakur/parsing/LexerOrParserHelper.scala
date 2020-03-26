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
    var lastMatch: PatternCase[Input, Helper] = _
    private var running = false
    var offset: Int = 0

    def run(): Unit = {
        while (hasNext) {
            //if (current.nonEmpty) println(current)
            proceed()
            update()
            println(lastMatch)
        }
    }

    def proceed(): Unit = {
        lastInput = getNext
        accumulate(current, lastInput)
        val stateCase = stateCases.find(sc => sc.state == currentState).getOrElse(
            stateCases.find(sc => sc.state == defaultState)
                .getOrElse(throw new Error("No state matches this!"))
        )
        var possibleFutureMatches = mutable.Set[PatternCase[Input, Helper]]()
        var lastMatch: (PatternCase[Input, Helper], Accumulator) = null
        var origPosition = offset
        var currentPosition = offset
        var temp: Input = lastInput
        //println(stateCase.patternCases)
        do {
            breakable {
                for (pc <- stateCase.patternCases) {
                    val matchRes = pc.pattern.tryMatch(current.asInstanceOf[Iterable[Input]])
                    matchRes match {
                        case NoMatch() => possibleFutureMatches -= pc
                        case FullMatch(couldMatchMore) =>
                            if (!couldMatchMore) {
                                this.lastMatch = pc
                                pc.action(this.asInstanceOf[lop.Helper])
                                current = emptyAccumulator()
                                print(lastMatch)
                                return
                            } else {
                                possibleFutureMatches += pc
                                lastMatch = (pc, current.asInstanceOf[mutable.Cloneable[Accumulator]].clone())
                                break
                            }
                        case NeedsMore() => possibleFutureMatches.addOne(pc)
                        //TODO handle this better
                        case PartialMatch(_) =>
                            throw new Error("This should not happen! " +
                                "There should have been a full match before!")
                    }
                }
            }
            if (possibleFutureMatches.isEmpty) {
                offset = currentPosition
                if (lastMatch == null)
                    throw new Error(s"Bad character($current) at position ${getPosition}")
                else {
                    val pc = lastMatch._1
                    pc.action(this.asInstanceOf[lop.Helper])
                    this.lastMatch = pc
                    current = lastMatch._2
                    return
                }
            } else {
                accumulate(current, getNext)
                currentPosition += 1
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
        accumulate(current)
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
