package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import collection.mutable.ListBuffer

//TODO add annotations
private object TemplateDefs {

  def field[_: P] = P(modifiers ~ typeRef ~ identifier ~ ("=" ~ expr ~ ";").?).map {
    case (mods, typ, name, initExpr) => new Field(name, ListBuffer(mods: _*), typ, initExpr)
  }

  /**
    * A method that is not a constructor
    */
  def normalMethod[_: P] = P()
  /**
    * A constructor
    */
  def ctor[_: P] = P()

  /**
    * A normal method or a constructor
    */
  def method[_: P] = P(modifiers ~ identifier ~ identifier.? ~ )

  def templateDefMember[_: P] = P(field | method)

  def classOrInterfaceBody[_: P] = P(Index ~ "{" ~ templateDefMember.rep ~ "}" ~ Index)
  def classOrInterface[_: P] = P(Index ~ StringIn("class", "interface") ~ Index ~ identifier ~ classOrInterfaceBody)

}
