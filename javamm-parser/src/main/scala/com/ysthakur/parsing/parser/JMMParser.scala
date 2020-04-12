package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.lexer.{Token, TokenType, ValidIdentifierTokenType, VariantToken}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

type Tok = Token[TokenType]

object JMMParser extends Parsers {
  override type Elem = Node
  
  private def validId: Parser[ValidIdNode] = Parser(
    (input: Input) => {
      input.first match {
        case tok: Token[?] => if (Token.isValidId(tok))
          Success(ValidIdNode(tok.asInstanceOf[Token[ValidIdentifierTokenType]]), input.rest)
        else Failure("Not a valid identifier", input)
        case _ => Failure("Not a token, much less a valid identifier", input)
      }
    })
}

class JMMReader(tokens: Seq[Tok]) extends Reader[Token[TokenType]] {
  override def first: Tok = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Tok] = new JMMReader(tokens.tail)
}

// import com.ysthakur.parsing.ast.Types.Node
// import com.ysthakur.parsing.grammar.{ConsPattern, LexerOrParser, LexerOrParserHelper, toState}
// import com.ysthakur.parsing.grammar.toState
// import com.ysthakur.parsing.lexer.KeywordTokenType._
// import com.ysthakur.parsing.lexer.SymbolTokenType._
// import com.ysthakur.parsing.lexer.Token
// import com.ysthakur.parsing.parser.ParserPatterns._

// import reflect.Selectable.reflectiveSelectable
// import scala.collection.mutable.ListBuffer

// /**
//   * TODO implement this
//   */
// object Parser extends LexerOrParser[Node, Node, Iterable[Node]]("TOPLEVEL") {
//   override type InputSource = ListBuffer[Node]
//   override type Helper      = Parser

//   //val x: CompositePattern[Node] = (PACKAGE - (validId) - SEMICOLON)

//   // "TOPLEVEL" := (
//   //     (PACKAGE - (validId) - SEMICOLON) --> { (h: Helper) =>
//   //       val lastMatch = h.lastMatch._2

//   //     }
//   // )

//   def createAST(tokens: Iterable[Token]): Node = {
//     process(new ListBuffer[Node]().addAll(tokens))
//   }

//   override def makeHelper(inputSource: InputSource): Parser = new Parser()
// }

// class Parser
//     extends LexerOrParserHelper[ListBuffer[Node], Node, Node, Iterable[Node]](
//         Parser
//     ) {

//   override def process(): Node = ???

//   /**
//     * Get the next character (if this is a lexer) or token (if this is a parser).
//     *
//     * @return The next piece of input or null if it's reached the end of the file
//     *         or input stream
//     */
//   override def getNext: Node = ???

//   /**
//     * Whether or not it can proceed
//     *
//     * @return
//     */
//   override def hasNext: Boolean = ???

//   override def accumulate(acc: Iterable[Node], input: Node): Unit = ???

//   /**
//     * Return the next character/token without consuming it.
//     *
//     * @return
//     */
//   override def peekNext: Node = ???

//   override def emptyAccumulator(): Iterable[Node] = ???
// }