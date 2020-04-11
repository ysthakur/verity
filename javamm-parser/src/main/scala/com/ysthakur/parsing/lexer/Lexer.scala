package com.ysthakur.parsing.lexer

import java.io.{BufferedInputStream, File, FileInputStream, FileWriter, IOException, InputStream}

import com.ysthakur.parsing._
import com.ysthakur.parsing.Match
import com.ysthakur.parsing.lexer.TokenType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.breakable

case class Position(var row: Int, var col: Int, var offset: Int) {
  def nextRow: Unit = {
    row += 1
    col = 0
  }
  def copy(): Position = Position(row, col, offset)
}

case class Lexer(file: BufferedInputStream, logFile: String = "./log.txt") {
  private[lexer] var lastToken: Token[_] = _
  private var position = Position(0, 0, 0)
  private val LOG = new FileWriter(File(logFile))

  def this(file: File) = this(new BufferedInputStream(new FileInputStream(file)))

  @throws[IOException]
  def getNext: Option[Char] = {
    val res = file.read()
    System.out.println(s"($res,${res.toChar})`")
    if (res == -1) None else Some(res.toChar)
  }

  def end(): Unit = {
    file.close()
  }
  
  def log(s: String): Unit = {
    LOG.write(s)
  }

  /**
    * The last match it had. Includes the [[com.ysthakur.parsing.grammar.PatternCase]] and the text
    * that was matched and a tuple in the format (`startOffset`, `endOffset`).
    * The end offset's not inclusive.
    */
  var lastMatch: Match[Char] = _
  var offset: Int = 0
  private[parsing] var lastInput: mutable.StringBuilder = StringBuilder()

  def tokenize(): Iterable[Token[?]] = {
    try {
      return tokenize_()
    } catch {
      case e: java.util.regex.PatternSyntaxException => {
        println("`" + e.getPattern + "`")
        end()
        throw e
      }
    } finally {
      println("asdkfja;sldjf;askljdf")
      end()
    }
  }

  /**
    *
    */
  @throws[BadCharacterError]
  def tokenize_(): Iterable[Token[?]] = {
    var lastInput = StringBuilder().append(getNext.getOrElse(throw new Error("File is empty!")))
    val tokens = ListBuffer[Token[?]]()
    val tokenTypes = JMMTokenTypes.allTokenTypes//.filter(_.isInstanceOf[KeywordTokenType])
    // print(tokenTypes)
    log(s"Last input = $lastInput, ${lastInput(0).toInt}")
    while (lastInput.nonEmpty ||
        (getNext match {
          case Some(next) =>
            log(s"Next = $next")
            lastInput = new StringBuilder().append(next)
            true
          case None => false
       })
    ) {
      val (matched: Match[Char], tokenType: TokenType, pos: Position, acc: StringBuilder) =
        tryMatch(tokenTypes, offset, lastInput).getOrElse(
            throw BadCharacterError(lastInput.head), position.row, position.col, this.offset)
      if (true/*!tokenType.isInstanceOf[IgnoredTokenType]*/) {
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
      this.position = pos.copy()
      lastInput = new StringBuilder(acc.substring(0))
      log(s"Matched = $matched, lastInput=`$lastInput`")
    }
    end()
    tokens
  }

  def tryMatch(
      tokenTypes: Iterable[TokenType],
      startOffset: Int,
      lastInput: StringBuilder
  ): Option[(Match[Char], TokenType, Position, StringBuilder)] = {
    val acc = StringBuilder(lastInput.toString)
    log(s"Entering, acc=`$acc`")

    var lastMatch: (Match[Char], TokenType)|Null = null
    var (lastMatchLength, currentLength) = (0, 1)
    var lastPos: Position = Position(position.row, position.col, offset)
    var lastChar: Char = -1.toChar
    
    var currentPos: Position = Position(position.row, position.col, offset)
    var origOffset = offset

    var possibleFutureMatches = mutable.Set[TokenType]()
    while ({
      breakable {
          for (tokenType <- tokenTypes)
            TokenTypeUtil.tryMatch(tokenType, acc, currentPos.offset) match {
              case FullMatch(matched: Match[Char], couldMatchMore: Boolean) => 
                {
                  log(s"Full match! $matched, tokentype=$tokenType")
                  if (lastMatchLength < currentLength) {
                    lastMatch = (matched, tokenType)
                    lastPos = currentPos.copy()
                    lastMatchLength = currentLength
                    log(s"Setting to lastMatch, currentLength=$currentLength")
                  }
                  if (couldMatchMore) possibleFutureMatches += tokenType
                }
              case `NeedsMore` => possibleFutureMatches.addOne(tokenType)
              case res => {
                if (res.isInstanceOf[PartialMatch[_]]) log(s"Partial match $res for tokentype=$tokenType")
                possibleFutureMatches -= tokenType
                if (lastMatch == res) lastMatch == null
              }
            }
      }
      
      log(s"Possible future matches = $possibleFutureMatches\n")
      if (possibleFutureMatches.isEmpty) {
        lastMatch match {
          case null =>
            throw new Exception(
                s"""Bad character(s) "$acc" at start offset $origOffset, $lastPos"""
            )
          case last: (Match[Char], TokenType) => {
            log("Matched! Found=\"" + last._2 + "\"")
            val matched = last._1
            acc.delete(0, matched.end - matched.start)
            log(s"Returning, acc = `$acc`")
            return Some((last._1, last._2, currentPos.copy(), acc))
          }
        }
      } else {
        val next = getNext.getOrElse(throw new Exception("Unexpected end of file!"))
        acc.append(next)
        log(s"Acc with next=$acc")
        currentPos.offset += 1
        if (lastChar == '\r') {
          if (next != '\n') {
            currentPos.row += 1
            currentPos.col = 0
          }
        } else if (lastChar == '\n') {
          currentPos.row += 1
          currentPos.col = 0
        } else currentPos.col += 1
        currentLength += 1
        lastChar = next
      }
      possibleFutureMatches.nonEmpty
    }) {}
    None
  }

  @throws[IOException]
  def getNext: Option[Char] = {
    val res = bis.read()
    log(s"($res,${res.toChar})`")
    if (res == -1) None else Some(res.toChar)
  }

  def end(): Unit = {
    bis.close()
    LOG.close()
  }

  private def log(msg: Any) = LOG.append(msg.toString).append('\n')
}

object Lexer {
  def tokenize(file: File): Iterable[Token[?]] = {
    val lexer = Lexer(new BufferedInputStream(new FileInputStream(file)))
    try {
      return lexer.tokenize()
    } catch {
      case e: Exception => {
        println("asdhfaksjdfhlaksjdhflakjsdhfkjashldfjkashdflkjashdf")
        lexer.end()
        throw e
      }
    } finally {
      println("asdkfja;sldjf;askljdf")
      lexer.end()
    }
  }
}
