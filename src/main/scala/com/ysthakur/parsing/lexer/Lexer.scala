package com.ysthakur.parsing.lexer

import java.io._

import com.ysthakur.parsing.grammar.{LexerOrParser, LexerOrParserHelper, Pattern, PatternCase, StateCase}

import scala.collection.mutable.ListBuffer

/**
  * Originally intended to a definition of the lexer with the DSL
  */
object Lexer
    extends LexerOrParser[Char, Iterable[Token], StringBuilder]("DEFAULT")
    with Dynamic {

  override type InputSource = InputStream
  override type Helper = Lexer
  private val action: ThisLOP#Helper => Unit = (helper: ThisLOP#Helper) => {
    val lastTokenType =
      patternCaseToTokenType(
          helper.lastMatch._1.asInstanceOf[PatternCase[Char, Helper]])
    helper.asInstanceOf[Helper].lastToken = lastTokenType match {
      case textTokenType: FixedTextTokenType =>
        new InvariantToken(textTokenType, helper.offset)
      case regexTokenType: RegexTokenType =>
        VariantToken(regexTokenType, helper.lastMatch._2)
      case e => throw new Error(s"Ahhh! Unrecognized token type $e")
    }
  }
  private val patternCaseToTokenType =
    JMMTokenTypes.allTokenTypes.map[PatternCase[Char, Helper], TokenType] {
      case (_, tokenType) =>
        PatternCase[Char, Helper](makePattern(tokenType), action) -> tokenType
    }

  def tokenize(file: File): Iterable[Token] =
    process(new BufferedInputStream(new FileInputStream(file)))

  {
    this.addStateCase(StateCase(firstState, patternCaseToTokenType.keys))
  }

  override def makeHelper(inputSource: InputStream): Lexer = Lexer(inputSource)

  private def makePattern(tt: TokenType): Pattern[Char] =
    tt match {
      case rtt: RegexTokenType     => RegexPattern(rtt.regex)
      case ftt: FixedTextTokenType => FixedTextPattern(ftt.text)
      case _                       => throw new Error("Unexpected type")
    }
}

/**
  * The thing that does the actual tokenizing
  *
  * @param file
  */
case class Lexer(file: InputStream)
    extends LexerOrParserHelper[InputStream,
                                Char,
                                Iterable[Token],
                                StringBuilder](Lexer) {
  private[lexer] val tokens = ListBuffer[Token]()
  private[lexer] var lastToken: Token = _

  private var passedRows, passedCols = 0

  override def process(): Iterable[Token] = {
    run()
    tokens
  }

  override def update(): Unit = {
    /*if (!lastToken.isInstanceOf[IgnoredTokenType])*/
    tokens += lastToken
    val text = lastToken.text
    val nRows = text.count(_ == '\n')
    passedRows += nRows
    val len = text.length
    //todo check if this is correct
    if (nRows > 0) passedCols = len - text.lastIndexOf('\n') - 2
    else passedCols += len
  }

  @throws[IOException]
  override def getNext: Char = {
    file.read().toChar
  }

  override def hasNext: Boolean = {
    try {
      val next = peekNext
      next.toInt != -1
    } catch {
      case _: Throwable => false
    }
  }

  @throws[IOException]
  override def peekNext: Char = {
    file.mark(4)
    val res = file.read()
    file.reset()
    if (res != -1) res.toChar else throw new Error("End of file!")
  }

  override def accumulate(acc: StringBuilder, input: Char): Unit =
    acc.addOne(input)

  override def emptyAccumulator(): StringBuilder = new StringBuilder()

  override def end(): Unit = {
    file.close()
  }

  override def getPosition: String =
    s"pointer offset=$offset, ($passedRows,$passedCols)"
}
