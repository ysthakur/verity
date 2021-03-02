package verity.parsing.parser

import language.implicitConversions

import verity.parsing._, ast._, infile._
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

  def typeRef[_ : P]: P[TypeRef] = P(identifier ~ "<" ~ typeArgList ~ ">").map {
    case (name, args) => TypeRef(name, args)
  }
  def wildCard[_ : P]: P[Wildcard] = P("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map {
    case (upper, lower) => Wildcard(upper, lower)
  }
  def typeArg[_ : P]: P[Type] = P(wildCard | typeRef)
  def typeArgList[_ : P] = argList(typeArg: P[Type])

  def file[_ : P] = P(expr)
}