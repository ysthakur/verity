package verity.parsing.parser

import collection.mutable
import collection.mutable.ArrayBuffer

import java.io.{File, FileInputStream, InputStreamReader, BufferedReader, EOFException}

import verity.parsing.TextRange
import verity.parsing.{Token, TokenType}

type ConfirmToken = () => Boolean
type Res = Option[Token]

final class Reader(file: File, encoding: String = "UTF-8") extends Iterator[Char] {
  /**
   * A list of offsets corresponding to chars. The char at `charInd` in `chars` has an offset given by
   * `offsets(charInd)`
   */
  private var offsets = ArrayBuffer[Int](0)

  var chars = new StringBuilder()

  override def toString = s"charInd:$charInd, offset:$offset, Chars:$chars, tokens:tokens, comments:$comments"

  /**
   * Single line commments, multiline comments, doc comments
   */
  private val comments = ArrayBuffer[Token]()

  var charInd = 0

  private var _offset = 0

//  def charInd = _charInd
//  private def charInd_=(newInd: Int) = _charInd = newInd
  def offset = _offset
  private def offset_=(offset: Int) = _offset = offset

  private val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))

  val fileEnd = file.length()

  private var _hasNext: Boolean = false
  
  {
    val firstChar = reader.read()
    _hasNext = firstChar != -1
    if (_hasNext) chars += firstChar.toChar
  }

  override def hasNext = _hasNext

  /**
   *
   * @param expectedType
   * @param matches
   * @param cut Whether or not it should discard the matched text if it matches
   * @param confirm
   * @return
   */
  def nextToken(
    expectedType: TokenType, 
    cut: Boolean, 
    after: => Boolean = true
  )(confirm: => Boolean): Res = {
    var startInd = charInd
    var startOffset = offset
    lazy val tok = Token(
      TextRange(startOffset, offset),
      chars.substring(startInd, charInd),
      expectedType
    )
    try {
      val confirmed = confirm
      val endInd = charInd
      val endOffset = offset
      if (confirmed && after) {
        val res = Some(Token(
          TextRange(startOffset, offset),
          chars.substring(startInd, endInd),
          expectedType
        ))
        if (cut) {
          cutMiddle(startInd, endInd)
          charInd = startInd
          offset = startOffset
        } else {
          charInd = endInd
          offset = endOffset
        }
        res
      } else { //roll it back
        charInd = startInd
        offset = startOffset
        None
      }
    } catch {
      case _: EOFException | _: ArrayIndexOutOfBoundsException =>
        throw new UnfinishedTokenException(tok)
    }
  }

  /**
   * Match a certain string as is (for symbolic operators, parens, etc.)
   */
  def nextToken(
                 expected: String,
                 expectedType: TokenType,
                 cut: Boolean
               ): Option[Token] = {
    println(s"Trying to match $expected, chars=$chars, charInd=$charInd")
    val res = nextToken(expectedType, cut) {
      expected.forall(c => hasNext && c == nextChar())
    }
    if (res != None) println("Matched!")
    res
  }

  /**
   * Find the next alphanumeric identifier. However, if it is a hard
   * keyword, the keyword will be added to the token buffer, and a
   * None will be returned. 
   */
  def nextAlphaNum(cut: Boolean): Option[Token] = {
    println("Trying to match alphanumreader="+this)
    val res = nextToken(TokenType.ALPHANUM, cut, !(hasNext && nextChar().isUnicodeIdentifierPart)) {
      if (hasNext) {
        val startInd = charInd
        var c = nextChar()
        if (c.isUnicodeIdentifierStart) {
          while (hasNext && c.isUnicodeIdentifierPart) c = nextChar()
          val diff = charInd - startInd
          //It shouldn't be a single underscore or a hard keyword 
          !((startInd + 1 == charInd && chars(startInd) == '_')
            || Token.hardKeywords.exists {
              keyword => keyword.length == diff && (0 until diff).forall(i => chars(i + startInd) == keyword(i))
            })
        } else false
      } else false
    }
    if (res != None) println("Matched alphanum, reader="+toString)
    res
  }

  def nextAlphaNum(text: String,
                   tokenType: TokenType = TokenType.ALPHANUM,
                   cut: Boolean = false): Option[Token] = {
    println(s"Trying to match alphanum text=$text,reader=$this")
    val res = nextToken(
      TokenType.ALPHANUM,
      cut,
      !(hasNext && nextChar().isUnicodeIdentifierPart)
    ) { text.forall(hasNext && _ == nextChar()) }
    if (res != None) println(s"Matched alphanum text=$text,reader=$this")
    res
  }
  
  //TODO fix this
  def isSymbol(char: Char): Boolean = 
    !(char.isWhitespace || char.isUnicodeIdentifierPart || char.isUnicodeIdentifierStart)
  
  def nextSymbol(cut: Boolean): Option[Token] = {
    nextToken(
      TokenType.SYMBOL,
      cut
    ) {
      var len = 0
      while (hasNext && isSymbol(nextChar())) len += 1
      len > 0
    }
  }

  def nextSymbol(symbol: String, cut: Boolean = false): Option[Token] = {
    nextToken(
      TokenType.SYMBOL,
      cut
    ) {
      var len = 0
      while (hasNext && isSymbol(nextChar())) len += 1
      len > 0
    }
  }

  /**
   * Find the next string literal.
   */
  def nextString(cut: Boolean): Option[Token] =
    nextToken(TokenType.STRING, cut) {
      hasNext && nextChar() == '"' && hasNext && {
        var prevC = '"'
        var nextC = nextChar()
        while (hasNext && nextC != '"' || prevC == '\\') {
          if (prevC == '\\') nextC match {
            case '\\' =>
              if (hasNext) {
                //Skip the next character because it doesn't matter to us
                nextC = nextChar()
              }
            case 't' | 'b' | 'n' | 'r' | 'f' | '\'' | '"' =>
            case c => syntaxError(s"Invalid escape in string: \\$c")
          }

          prevC = nextC
          nextC = nextChar()
        }

        hasNext || (nextC == '"' && prevC != '\\')
      }
    }

  //TODO!!!!
  def nextNumLiteral(cut: Boolean): Option[Token] = {
    nextToken(TokenType.NUM_LITERAL, cut) {
      if (hasNext) {
        var c = nextChar()
        if (c.isDigit || c == '+' || c == '-') {
          if (!c.isDigit && (!hasNext || !nextChar().isDigit)) false
          else {
            while (hasNext && (c.isDigit || c == '_')) c = nextChar()
            if (c == '.') {
              while (hasNext && (c.isDigit || c == '_')) c = nextChar()
            }
            c match {
              case 'L' | 'l' => {}
            }
            true
          }
        } else false
      } else false
    }
  }

  /**
   * Skip comments (including documentation comments) and whitespace
   */
  @annotation.tailrec
  def skipCommentsAndWS(): Unit = {
    if (/*!hasTokens && */hasNext) {
      var c = nextChar()
      while (hasNext && (c == ' ' || c == '\t')) c = nextChar()

      val startOffset = offset
      val startInd = charInd

      if (c != ' ' && c != '\t') {
        if (c != '/' || !hasNext) {
          //Definitely not a comment
          //Back up because this character will be needed later
          prevChar()
        } else { //Possible comment
          nextChar() match {
            case '/' =>
              //Single line comment
              val singleLineCommentError = () => new UnfinishedTokenException(
                Token(
                  TextRange(startOffset, offset),
                  chars.substring(startInd, charInd),
                  TokenType.SINGLE_LINE_COMMENT
                ))
              while (hasNext && c != '\n' && c != '\r') c = nextChar(singleLineCommentError)
              if (hasNext && c == '\r' && nextChar(singleLineCommentError) != '\n') prevChar()

              //Keep track of this comment
              comments += Token(
                TextRange(startOffset, offset),
                chars.substring(startInd, charInd),
                TokenType.SINGLE_LINE_COMMENT
              )
              //Keep skipping comments and whitespace
              skipCommentsAndWS()
            case '*' =>
              //Multiline comment, possibly doc comment
              val multilineCommentError = () => new UnfinishedTokenException(
                Token(
                  TextRange(startOffset, offset),
                  chars.substring(startInd, charInd),
                  TokenType.MULTILINE_COMMENT
                ))
              var prevC = '*'
              var nextC = nextChar(multilineCommentError)
              while (prevC != '*' || nextC != '/') {
                prevC = nextC
                nextC = nextChar(multilineCommentError)
              }

              //Keep track of this comment
              comments += Token(
                TextRange(startOffset, offset),
                chars.substring(startInd, charInd),
                //Differentiate between /** and /* comments
                if (charInd - startInd > 4 && chars(startInd + 2) == '*') TokenType.DOC_COMMENT
                else TokenType.MULTILINE_COMMENT
              )
              //Keep skipping comments and whitespace
              skipCommentsAndWS()
            case _ =>
              //Not a comment, so back up
              prevChar()
              prevChar()
          }
        }
      }
    }
  }

  /**
   * Discard all read characters after the given index
   */
  def keepUntil(newEnd: Int): Unit = {
    chars = chars.slice(0, newEnd)
    offsets = offsets.takeInPlace(newEnd)
    charInd = newEnd
  }

  /**
   * Discard everything up to the given index
   *
   * @param newStart
   */
  def keepAfter(newStart: Int): Unit = {
    chars = chars.slice(newStart, chars.size)
    offsets = offsets.dropInPlace(newStart)
    charInd = 0
  }

  /**
   * Make a cut in the middle of `chars`. Discard everything between
   * `newStart` and `newEnd`
   * @param newStart
   * @param newEnd
   */
  def cutMiddle(newStart: Int, newEnd: Int): Unit = {
    chars = chars.slice(0, newStart).append(chars.slice(newEnd, chars.size))
    offsets.remove(newStart, newEnd - newStart)
    offsets.trimToSize()
    charInd -= newEnd - newStart
  }

  def syntaxError(msg: String) = throw new Error(s"Syntax error: $msg")

  def prevChar(): Unit = {
    charInd -= 1
    offset -= 1
    chars(charInd)
  }

  def peekChar: Option[Char] =
    Option.when(charInd < chars.size)(chars(charInd))
    
  def nextOrEmpty = peekChar.fold(Token.empty(-1))(c => Token(TextRange.empty(-1), "" + c))
  
  override def next() = nextChar()

  /**
   * Get the offset at the given index in `chars`
   * @param charInd
   */
  def getOffsetAt(charInd: Int) = {
    offsets(charInd)
  }

  /**
   * Get the next character, and read one if we're at the end of the StringBuilder
   */
  def nextChar(throwErr: () => Throwable = () => EOFException()): Char = {
    if (!hasNext || charInd == chars.size) throwErr()
    val res = chars(charInd)
//    val stream = new java.io.ByteArrayOutputStream()
//    Console.withOut(stream) {
      //all printlns in this block will be redirected
      println("Reading " + res)
//    }
    charInd += 1
    offset += 1
    if (charInd == chars.size) {
      val next = reader.read()
      if (next == -1) _hasNext = false
      else {
        chars += next.toChar
        offsets += offset
      }
    }
    res
  }
}