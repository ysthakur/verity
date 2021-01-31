package verity.parsing.parser

import verity.CompilationError
import verity.parsing.ast.infile.Node
// import verity.parsing.lexer._

import scala.collection.mutable.ListBuffer

object Parser {
  @throws[CompilationError]
  def parse(reader: Reader): Node = {
    val testExpr = ParserPatterns.expr
    println("In Parser.parse")
    testExpr.tryMatch(reader, true) match {
      case Failed(got, exp, pos, _) => throw ParseError(exp, got, pos)
      case Matched(create, rest, _) => create()
//        rest.skipCommentsAndWS()
//        if (rest.isEmpty) create()
//        else throw CompilationError(msg=s"Matched ${create().text} \nbut could not match $rest")
    }
  }
}