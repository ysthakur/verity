package verity.parser

import verity.ast._, infile._
import Core._
import Exprs._
import Methods._

import fastparse._, JavaWhitespace._

import collection.mutable.ListBuffer

//TODO add annotations
private object Classlikes {

  def field[_: P] = P(modifiers ~ typeRef ~ identifier ~ ("=" ~/ expr).? ~ ";").map {
    case (mods, typ, name, initExpr) => new Field(name, mods.to(ListBuffer), typ, initExpr)
  }

  //TODO field or method
  def templateDefMember[_: P]: P[Any] = P((normMethod: P[Any]) | (ctor: P[Any]) | (field: P[Any]))

  def classOrInterfaceBody[_: P] = P(Index ~ "{" ~ templateDefMember.rep ~ "}" ~ Index)

  //TODO add modifiers and annotations
  def clazz[_: P] = P(
      modifiers ~ Index ~ "class" ~/ Index ~ identifier ~ typeParamList.? ~ classOrInterfaceBody
  ).map {
    case (modifiers, classTokStart, classTokEnd, name, typeParams, (braceStart, members, braceEnd)) =>
      val (fields, normMethodsAndCtors) = members.partition(_.isInstanceOf[Field])
      val (normMethods, ctors) = normMethodsAndCtors.partition(_.isInstanceOf[NormMethod])
      lazy val cls: ClassDef = new ClassDef(
          ListBuffer(), //todo annotations
          modifiers.to(ListBuffer),
          name,
          typeParams.getOrElse(TypeParamList.empty),
          fields.to(ListBuffer).asInstanceOf[ListBuffer[Field]],
          ctors.map(_.asInstanceOf[(=> Classlike) => Constructor](cls)).to(ListBuffer),
          normMethods.to(ListBuffer).asInstanceOf[ListBuffer[NormMethod]],
          TextRange(classTokStart, classTokEnd),
          TextRange(braceStart, braceEnd)
      )
      cls
  }

  //todo enums, interfaces, annotation definitions
  def classlike[_: P]: P[Classlike] = P(clazz)

}
