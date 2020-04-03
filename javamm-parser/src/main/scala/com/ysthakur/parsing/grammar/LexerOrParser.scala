package com.ysthakur.parsing.grammar

import com.ysthakur.parsing.grammar.State
import com.ysthakur.util.as
import com.ysthakur.util.utils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.dynamics

/**
  * An (unnecessary) abstraction over lexers and parsers
  *
  * @tparam Input       The type of the input (character, token)
  * @tparam Accumulator The data structure all the pieces of the input are stored in
  *                     (a StringBuilder or ASTNode)
  */
@Deprecated
abstract class LexerOrParser[Input, Output, Accumulator <: Iterable[Input]]
    (val firstState: String) {

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
  type _Match_ = Match[Input]
  type _PatternCase_ = PatternCase[Input, Helper]
  type _Iterable_ = Iterable[Input]

  implicit val thisTokenizer: ThisLOP      = this
  private[parsing] val stateCases: =
    mutable.LinkedHashMap[StateCase[Input, Helper]]()
  /**
    * A bunch of helpers that might be processing files/tokens at once
    */
  private val helpers = mutable.Set[Helper]()
  val catchAllState = "ANY"

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

  def addState(stateCase: (String, Iterable[PatternCase[Input, Helper]])): Unit =
    stateCases.addOne(stateCase)

  def makeHelper(inputSource: InputSource): Helper
}
