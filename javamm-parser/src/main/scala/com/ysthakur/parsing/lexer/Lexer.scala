package com.ysthakur.parsing.lexer

import java.io.{BufferedInputStream, File, FileInputStream, IOException, InputStream}

import com.ysthakur.parsing.grammar.{FullMatch, Match, NeedsMore, NoMatch, PartialMatch}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.breakable

object Lexer {
  def tokenize(file: File): Iterable[Token] =
    Lexer(new BufferedInputStream(new FileInputStream(file))).tokenize()
}

case class Lexer(file: InputStream) {
  type Position = (Int, Int)

  private[lexer] var lastToken: Token = _
  private var rows, cols = 0

  @throws[IOException]
  def getNext: Option[Char] = {
    val res = file.read()
    if (res == -1) None else Some(res.toChar)
  }

  def hasNext: Boolean = {
    try {
      val next = peekNext
      next.toInt != -1
    } catch {
      case _: Throwable => false
    }
  }

  @throws[IOException]
  def peekNext: Char = {
    file.mark(4)
    val res = file.read()
    file.reset()
    if (res != -1) res.toChar else throw new Error("End of file!")
  }

  def accumulate(acc: StringBuilder, input: Char): Unit =
    acc.addOne(input)

  def end(): Unit = {
    file.close()
  }

  def getPosition: String =
    s"pointer offset=$offset, ($rows,$cols)"

  /**
    * The last match it had. Includes the [[com.ysthakur.parsing.grammar.PatternCase]] and the text
    * that was matched and a tuple in the format (`startOffset`, `endOffset`).
    * The end offset's not inclusive.
    */
  var lastMatch: Match[Char] = _
  var offset: Int = 0
  private[parsing] var lastInput: mutable.StringBuilder = StringBuilder()

  /**
    *
    */
  @throws[BadCharacterError]
  def tokenize(): Iterable[Token] = {
    if (!hasNext) throw new Error("Empty input!!!")
    val tokens = ListBuffer[Token]()
    while (lastInput.nonEmpty ||
        (getNext match {
          case Some(next) =>
            lastInput = new StringBuilder(next)
            true
          case None => false
       })
    ) {
      val (matched: Match[Char], tokenType: TokenType, pos: Position) =
        tryMatch(JMMTokenTypes.allTokenTypes.map(_._2), this.offset).getOrElse(
            throw BadCharacterError(lastInput.head), rows, cols, this.offset)
      if (!tokenType.isInstanceOf[IgnoredTokenType]) {
        tokens.addOne(tokenType match {
          case textTokenType: FixedTextTokenType =>
            InvariantToken(
              textTokenType,
              matched.start,
              matched.end)
          case regexTokenType: RegexTokenType =>
            VariantToken(
              regexTokenType,
              matched.matched.toString,
              matched.start,
              matched.end)
        })
      }
      this.rows = pos._1
      println(matched)
    }
    tokens
  }

  def tryMatch(
      tokenTypes: Iterable[TokenType],
      startOffset: Int
  ): Option[(Match[Char], TokenType, Position)] = {
    val acc = StringBuilder()
    var possibleFutureMatches = mutable.Set[TokenType]()
    var lastMatch: (Match[Char], TokenType) = null
    var (lastMatchLength, currentLength) = (0, 1)
    var lastPos: (Int, Int) = (rows, cols)
    var currentPos: (Int, Int) = (rows, cols)
    var lastChar: Char = -1.toChar
    var origOffset, currentOffset = offset
    while ({
      breakable {
          val input = acc.toString
          for (tokenType <- tokenTypes)
            TokenTypePattern.tryMatch(tokenType, input, currentOffset) match {
              case FullMatch(matched: Match[Char], couldMatchMore) =>
                if (lastMatchLength < currentLength) {
                  lastMatch = (matched, tokenType)
                  lastPos = currentPos
                  lastMatchLength = currentLength
                }
                if (couldMatchMore) possibleFutureMatches += tokenType
              case NeedsMore()                 => possibleFutureMatches.addOne(tokenType)
              case res => {
                possibleFutureMatches -= tokenType
                if (lastMatch == res) lastMatch == null
              }
            }
      }

      if (possibleFutureMatches.isEmpty) {
        if (lastMatch == null)
          throw new Exception(
              s"""Bad character(s) "$acc" at start offset $origOffset, ${getPosition}"""
          )
        else {
          println(
              "Matched! Found=\"" + lastMatch._2 + "\""
          )
          return Some((lastMatch._1, lastMatch._2, (currentPos)))
        }
      } else {
        val next = getNext.getOrElse(throw new Exception("Unexpected end of file!"))
        accumulate(acc, next)
        currentOffset += 1
        val (row, col) = currentPos
        currentPos =
            if (lastChar == '\r') {
              if (next == '\n') currentPos
              else (row + 1, 0)
            } else if (lastChar == '\n') (row + 1, 0)
            else (row, col + 1)
        currentLength += 1
        lastChar = next
      }
      possibleFutureMatches.nonEmpty
    }) {}
    None
  }

}
