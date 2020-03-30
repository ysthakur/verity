package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.{LexerOrParser, LexerOrParserHelper}
import com.ysthakur.parsing.lexer.KeywordTokenTypes._
import com.ysthakur.parsing.lexer.SymbolTokenTypes._
import com.ysthakur.parsing.lexer.VariantTextTokenTypes._
import com.ysthakur.parsing.lexer.Token
import com.ysthakur.parsing.parser.ParserPatterns._

import scala.collection.mutable.ListBuffer

/**
  * TODO implement this
  */
object Parser extends LexerOrParser[Node, Node, Iterable[Node]]("TOPLEVEL") {
  override type InputSource = ListBuffer[Node]
  override type Helper = Parser

  s.TOPLEVEL := (
      (PACKAGE-(validId)) --> { (h: Helper) =>
       }
  )

  def createAST(tokens: Iterable[Token]): Node = {
    process(new ListBuffer[Node]().addAll(tokens))
  }

  override def makeHelper(inputSource: InputSource): Parser = new Parser()
}

class Parser
    extends LexerOrParserHelper[ListBuffer[Node], Node, Node, Iterable[Node]](
        Parser) {

  override def process(): Node = ???

  /**
    * Get the next character (if this is a lexer) or token (if this is a parser).
    *
    * @return The next piece of input or null if it's reached the end of the file
    *         or input stream
    */
  override def getNext: Node = ???

  /**
    * Whether or not it can proceed
    *
    * @return
    */
  override def hasNext: Boolean = ???

  override def accumulate(acc: Iterable[Node], input: Node): Unit = ???

  /**
    * Return the next character/token without consuming it.
    *
    * @return
    */
  override def peekNext: Node = ???

  override def emptyAccumulator(): Iterable[Node] = ???
}
