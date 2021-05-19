package verity.parser

import verity.ast._, infile._
import Core._
import Exprs._
import Methods._
import Types._

import fastparse._, JavaWhitespace._

import collection.mutable.ArrayBuffer

//TODO add annotations
private object Classlikes {

  // def field[_: P] = P(modifiers ~ nonWildcardType ~ identifierText ~ ("=" ~/ expr).? ~ ";").map {
  //   case (mods, typ, name, initExpr) => new Field(name, mods.to(ArrayBuffer), typ, initExpr)
  // }

  def methodOrField[_: P]: P[Seq[Modifier] => Any] =
    P(returnType ~ identifierText ~/ (field2 | methodWithoutTypeParams)).map {
      case (typ, text, fieldOrMethod) => fieldOrMethod(typ, text)
    }

  def field2[_: P]: P[(Type, Text) => Seq[Modifier] => Any] =
    P(("=" ~/ expr).? ~ ";").map { initExpr =>
      (typ: Type, name: Text) => (modifiers: Seq[Modifier]) => new Field(name, modifiers.to(ArrayBuffer), typ, initExpr)
    }

  //field or method
  // def templateDefMember[_: P]: P[Any] = P((normMethod: P[Any]) | (ctor: P[Any]) | (field: P[Any]))

  def templateDefMember2[_: P]: P[Any] =
    P(modifiers ~ (methodWithTypeParams | ctor | methodOrField : P[Seq[Modifier] => Any])).map { 
      case (mods, astCtor) => astCtor(mods)
    }

  def classOrInterfaceBody[_: P]: P[(Int, Seq[Any], Int)] = P("{" ~/ Index ~ templateDefMember2.rep ~ "}" ~ Index)

  //TODO add modifiers and annotations
  //TODO allow extending classes, interfaces
  def clazz[_: P]: P[ClassDef] = P(
      modifiers ~ "class" ~/ Index ~ identifier ~ typeParamList.? ~ classOrInterfaceBody
  ).map {
    case (modifiers, classTokEnd, name, typeParams, (braceStart, members, braceEnd)) =>
      val (fields, normMethodsAndCtors) = members.partition(_.isInstanceOf[Field])
      val (normMethods, ctors) = normMethodsAndCtors.partition(_.isInstanceOf[NormMethod])
      lazy val cls: ClassDef = new ClassDef(
          ArrayBuffer(), //todo annotations
          modifiers.to(ArrayBuffer),
          name,
          typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
          null, //todo superclass
          null, //todo
          fields.to(ArrayBuffer).asInstanceOf[ArrayBuffer[Field]],
          ctors.map(_.asInstanceOf[(() => Classlike) => Constructor](() => cls)).to(ArrayBuffer),
          normMethods.to(ArrayBuffer).asInstanceOf[ArrayBuffer[NormMethod]],
          TextRange(classTokEnd - 5, classTokEnd),
          TextRange(braceStart, braceEnd)
      )

      cls
  }

  //todo enums, interfaces, annotation definitions
  def classlike[_: P]: P[Classlike] = P(clazz)

}
