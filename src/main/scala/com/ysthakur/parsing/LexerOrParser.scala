package com.ysthakur.parsing

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * An (unnecessary) abstraction over lexers and parsers
 *
 * @param stateNames The names of the states this tokenizer/parser will have
 * @tparam Input       The type of the input (character, token)
 * @tparam Accumulator The data structure all the pieces of the input are stored in
 *                     (a StringBuilder or ASTNode)
 */
abstract class LexerOrParser[Input, Output, Accumulator <: Iterable[Input]]
(stateNames: String*) extends Dynamic {

    type This = LexerOrParser[Input, Output, Accumulator]
    type I = Input
    type O = Output
    type InputSource
    type Helper <: LexerOrParserHelper[InputSource, Input, Output, _ <: Accumulator]

    val defaultState: State = State("DEFAULT")
    private[parsing] val stateCases: ListBuffer[StateCase[Input, Helper]] = new ListBuffer()
    /**
     * A bunch of helpers that might be processing files/tokens at once
     */
    private val helpers = mutable.Set[Helper]()

    implicit val states: Map[String, State] = makeStates(stateNames)
    implicit val thisTokenizer: This = this

    def makeHelper(inputSource: InputSource): Helper

    /**
     * Tokenize or parse the input file or list of tokens
     * @param inputSource
     * @return
     */
    final def process(inputSource: InputSource): Output = {
        val helper = makeHelper(inputSource)
        helpers.add(helper)
        helper.process()
    }

    private def makeStates(stateNames: Seq[String]): Map[String, State] =
        stateNames.map(name => (name, State(name))).appended((defaultState.name, defaultState)).toMap

    def addStateCase(stateCase: StateCase[I, Helper]): Unit = stateCases.addOne(stateCase)

    /**
     * Just to get a state by its name.
     */
    protected val s: Dynamic {def selectDynamic(stateName: String): State} = new Dynamic {
        def selectDynamic(stateName: String): State =
            states.getOrElse(stateName, throw new NullPointerException())
    }
}