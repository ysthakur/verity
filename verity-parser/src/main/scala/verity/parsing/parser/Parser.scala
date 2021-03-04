package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import java.io.FileInputStream

object Parser {
  def parseFile(input: FileInputStream) = {
    import Parsed._
    parse(input, file(_)) match {
      case foo => println(foo)
    }
  }

  def file[_: P] = P(expr)
}
