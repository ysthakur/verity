package verity.parsing.parser

import language.implicitConversions

import verity.parsing._
import verity.ast._, infile._
import Core._
import Exprs._
import Methods._

import fastparse._, JavaWhitespace._

import collection.mutable.ListBuffer

//TODO add annotations
private object TemplateDefs {

  def field[_: P] = P(modifiers ~ typeRef ~ identifier ~ ("=" ~/ expr).?  ~ ";").map {
    case (mods, typ, name, initExpr) => new Field(name, mods.to(ListBuffer), typ, initExpr)
  }

  //TODO field or method
  def templateDefMember[_: P] = P(normMethod | field)

  def classOrInterfaceBody[_: P] = P(Index ~ "{" ~ templateDefMember.rep ~ "}" ~ Index)
  
  //TODO add modifiers and annotations
  //todo enums, interfaces, annotation definitions
  def classlike[_: P] = P(modifiers ~ Index ~ StringIn("class", "interface").! ~ Index ~ identifier ~ classOrInterfaceBody).map {
    case (modifiers, metaclassStart, metaclass, metaclassEnd, name, (braceStart, members, braceEnd)) =>
      val (fields, methods) = members.partition(_.isInstanceOf[Field])
      ClassDef(
        ListBuffer(), 
        modifiers.to(ListBuffer),
        ClasslikeType.CLASS(TextRange(-1, -1)),
        name,
        fields.to(ListBuffer).asInstanceOf[ListBuffer[Field]],
        methods.to(ListBuffer).asInstanceOf[ListBuffer[Method]],
        TextRange(-1, -1)
      )
  }

}
