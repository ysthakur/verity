package com.ysthakur.parsing.grammar

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.dynamics
import scala.reflect.Selectable

/**
  * An (unnecessary) abstraction over lexers and parsers
  *
  * @tparam Input       The type of the input (character, token)
  * @tparam Accumulator The data structure all the pieces of the input are stored in
  *                     (a StringBuilder or ASTNode)
  */
abstract class LexerOrParser[Input, Output, Accumulator <: Iterable[Input]](
    val firstState: String
) extends Dynamic {

  type ThisLOP = LexerOrParser[Input, Output, Accumulator]
  type I       = Input
  type O       = Output
  type InputSource
  type Helper <: LexerOrParserHelper[
      InputSource,
      Input,
      Output,
      _ <: Accumulator
  ]

  /**
    * Just to get a state by its name.
    */
  protected val s: Selectable { def selectDynamic(stateName: String): State } =
    new Dynamic {
      def selectDynamic(stateName: String): State =
        new State(
            states
              .find(_ == stateName)
              .getOrElse(throw new Error(s"No such state found: `$stateName`"))
        )
    }

  implicit val states: mutable.Set[String] = mutable.Set()
  implicit val thisTokenizer: ThisLOP      = this
  private[parsing] val stateCases: ListBuffer[StateCase[Input, Helper]] =
    new ListBuffer()

  /**
    * A bunch of helpers that might be processing files/tokens at once
    */
  private val helpers = mutable.Set[Helper]()

  def makeHelper(inputSource: InputSource): Helper

  /**
    * Tokenize or parse the input file or list of tokens
    *
    * @param inputSource
    * @return
    */
  final def process(inputSource: InputSource): Output = {
    val helper = makeHelper(inputSource)
    helpers.add(helper)
    val res = helper.process()
    helpers.remove(helper)
    res
  }

  def addStateCase(stateCase: StateCase[I, Helper]): Unit =
    stateCases.addOne(stateCase)

  private def makeStates(stateNames: Seq[String]): Set[String] =
    stateNames.map(name => name).appended(firstState).toSet
}
