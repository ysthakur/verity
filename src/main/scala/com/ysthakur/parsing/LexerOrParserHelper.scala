package com.ysthakur.parsing

import com.ysthakur.util._

import scala.collection.mutable.ListBuffer

abstract class LexerOrParserHelper[InputSource, Input, Output, Accumulator <: Iterable[Input]]
(val lop: LexerOrParser[Input, Output, Accumulator], val current: Accumulator) {

    import lop.{This => d, _}

    var currentState: State = defaultState
    private[parsing] var lastInput: Input = _
    var lastMatch: PatternCase[Input, Helper] = _
    private var running = false

    def run(): Unit = {
        while (hasNext) {
            proceed()
        }
    }

    def proceed(): Unit = {
        lastInput = getNext
        accumulate(lastInput)
        val stateCase = stateCases.find(sc => sc.state == currentState).getOrElse(
            stateCases.find(sc => sc.state == defaultState)
                .getOrElse(throw new Error("No state matches this!"))
        )
        var possibleFutureMatches = ListBuffer[PatternCase[Input, Helper]]()
        var lastMatch: PatternCase[Input, Helper] = null
        do {
            //val pattern = findPattern(stateCase.patternCases.map(pc => pc.pattern))
            for (pc <- stateCase.patternCases) {
                val matchRes = pc.pattern.tryMatch(current.asInstanceOf)
                matchRes match {
                    case NoMatch() => possibleFutureMatches -= pc
                    case FullMatch(couldMatchMore) =>
                        if (!couldMatchMore) {
                            this.lastMatch = pc
                            pc.action(this.asInstanceOf[lop.Helper])
                            possibleFutureMatches = ListBuffer.empty
                            lastMatch = null
                        } else {
                            possibleFutureMatches :+= pc
                            lastMatch = lastMatch ?: pc
                        }
                    case NeedsMore() => possibleFutureMatches.addOne(pc)
                    //TODO handle this better
                    case PartialMatch(_) =>
                        throw new Error("This should not happen! " +
                            "There should have been a full match before!")
                }
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

    /**
     * Add this piece of the input (character/token) to the
     * accumulator, which is a StringBuilder or something
     */
    def accumulate(input: Input): Unit

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

    protected def end(): Unit = {}

}
