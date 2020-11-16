package verity.parsing.parser

import collection.mutable.ArrayBuffer

import java.io.{File, FileInputStream, InputStreamReader, BufferedReader, EOFException}

import verity.parsing.TextRange
import verity.parsing.{Token, TokenType}

type ConfirmToken = () => Boolean
// type Res = Option[Either[Token, Token]]
type Res = Option[Token]

class Reader(file: File, encoding: String) extends Iterator[Char]:

  /**
   * Tokens that have already been read
   */
  private val tokens = ArrayBuffer[Token]()

  private var chars = new StringBuilder()
  /**
   * Single line commments, multiline comments, doc comments
   */
  private val comments = ArrayBuffer[Token]()
  
  private var charInd, tokenInd = -1
  private var arrInd = -1
  // private var sliceInd = -1
  // private var row, col = -1

  private var _offset = -1
  def offset = _offset
  private def offset_=(offset: Int) = _offset = offset

  private val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
  
  val fileEnd = file.length()

  private[this] val firstChar = reader.read()
  if (firstChar != -1) chars += firstChar.toChar

  private var _hasNext: Boolean = firstChar != -1
  override def hasNext = _hasNext

  // def isEmpty: Boolean = !hasNext && !hasTokens

  // def nextToken(): Option[Token] =
  //   Option.when(tokenInd < tokens.size){
  //     tokenInd += 1
  //     tokens(tokenInd - 1)
  //   }

  def nextToken(expectedType: TokenType, matches: Token => Boolean)(confirm: => Boolean): Res = {
    if (tokenInd < tokens.size) {
      val tok = tokens(tokenInd)
      if (matches(tok)) {
        tokenInd += 1
        Some(tok)
      } else None
    } else {
      var startInd = charInd
      var startOffset = offset
      lazy val tok = Token(
          TextRange(startOffset, offset),
          chars.substring(startInd, charInd),
          expectedType
        )
      try {
        if (confirm) Some(tok)
        else {
          //roll it back
          charInd = startInd
          offset = startOffset
          None
        }
      } catch {
        case _: EOFException | _: ArrayIndexOutOfBoundsException => 
          throw new UnfinishedTokenException(tok)
      }
      // tokens += res
    }
  }

  /**
   * Match a certain string as is (for symbolic operators, parens, etc.)
   */
  def nextToken(
      expected: String, 
      expectedType: TokenType, 
      matchesTok: Token => Boolean = _ => true
    ): Option[Token] =
    nextToken(expectedType, matchesTok){ expected.forall(c => hasNext && c == nextChar()) }

  /**
   * Find the next alphanumeric identifier. However, if it is a hard
   * keyword, the keyword will be added to the token buffer, and a
   * None will be returned. 
   */
  def nextAlphaNum(): Option[Token] =
    nextToken(TokenType.ALPHANUM, _ == TokenType.ALPHANUM){ 
      if (hasNext) {
        val startInd = charInd
        var c = nextChar()
        if (c.isLetter || c == '$' || c == '_') {
          while (hasNext && (c.isLetterOrDigit || c == '$' || c == '_')) c = nextChar()
          
          val diff = charInd - startInd
          //It shouldn't be a single underscore or a hard keyword 
          (startInd + 1 != charInd || chars(startInd) != '_') ||
            Token.hardKeywords.exists { 
              keyword => keyword.length == diff && (0 until diff).forall(i => chars(i + startInd) == keyword(i))
            }
        } else false
      } else false
    }

  /**
   * Find the next string literal.
   */
  def nextString(): Option[Token] =
    nextToken(TokenType.STRING, _ == TokenType.STRING){
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
  def nextNumLiteral(): Option[Token] = {
    nextToken(TokenType.NUM_LITERAL, _ == TokenType.NUM_LITERAL) {
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
  private def skipCommentsAndWS(): Unit =
    if (!hasTokens && hasNext) {
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

  def hasTokens: Boolean = tokenInd < tokens.size

  def syntaxError(msg: String) = throw new Error(s"Syntax error: $msg")

  def prevChar(): Unit = {
    charInd -= 1
    offset -= 1
    chars(charInd)
  }

  override def next() = nextChar()

  /**
   * Get the next character, and read one if we're at the end of the StringBuilder
   */
  def nextChar(throwErr: () => Throwable = () => EOFException()): Char = {
    if (!hasNext || charInd == chars.size) throwErr()
    val res = chars(charInd)
    charInd += 1
    offset += 1
    if (charInd == chars.size) {
      val next = reader.read()
      if (next == -1) _hasNext = false
      else chars += next.toChar
    }
    res
    // char match {
      // case -1 => throwErr()
      // case '\r' => 
      //   row += 1
      //   col = 0
      // case '\n' =>
      //   if (charInd == 0 || chars(charInd - 1) != '\r') {
      //     row += 1
      //     col = 0
      //   }
    // }
  }
   
  /**
   * Read a character and store it in the buffer
   *
   */
  // private def readChar() = {
  //   val char = reader.read()
  //   if (char == -1) {
  //     _hasNext = false
  //   } else {
  //     chars += char.toChar
  //   }
  // }

end Reader