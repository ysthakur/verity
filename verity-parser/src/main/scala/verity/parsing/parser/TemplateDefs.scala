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

  def field[_: P] = P(modifiers ~ typeRef ~ identifier ~ ("=" ~ expr ~ ";").?).map {
    case (mods, typ, name, initExpr) => new Field(name, ListBuffer(mods: _*), typ, initExpr)
  }

  //TODO field or method
  def templateDefMember[_: P] = P(normMethod)

  def classOrInterfaceBody[_: P] = P(Index ~ "{" ~ templateDefMember.rep ~ "}" ~ Index)
  
  //TODO add modifiers and annotations
  def classOrInterface[_: P] = P(modifiers ~ Index ~ StringIn("class", "interface").! ~ Index ~ identifier ~ classOrInterfaceBody).map {
    case (modifiers, metaclassStart, metaclass, metaclassEnd, name, (braceStart, members, braceEnd)) =>
      ClassDef(
        ListBuffer(), 
        ListBuffer(modifiers: _*),
        TemplateDefType.CLASS(TextRange(-1, -1)),
        name,
        ListBuffer(),
        ListBuffer(members.asInstanceOf[Seq[Method]].filter(_.isInstanceOf[Method]): _*).asInstanceOf[ListBuffer[Method]],
        TextRange(-1, -1)
      )
  }

}
