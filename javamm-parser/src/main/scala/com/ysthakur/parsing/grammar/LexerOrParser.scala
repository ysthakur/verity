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

  implicit val thisTokenizer: ThisLOP      = this
  private[parsing] val stateCases: mutable.LinkedHashMap[String, Iterable[PatternCase[Input, Helper]]] =
    mutable.LinkedHashMap()
  /**
    * A bunch of helpers that might be processing files/tokens at once
    */
  private val helpers = mutable.Set[Helper]()


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
