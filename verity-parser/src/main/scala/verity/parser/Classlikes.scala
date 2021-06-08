package verity.parser

import verity.ast._, infile._
// import Core._
// import Exprs._
// import Methods._
// import Types._
import Parser.ps2tr

import fastparse._, JavaWhitespace._

import collection.mutable.ArrayBuffer

//TODO add annotations
private class Classlikes(core: Core, types: Types, exprs: Exprs, methods: Methods)(implicit offsetToPos: ArrayBuffer[(Int, Int, Int)]) {
  import core._
  import exprs._
  import methods._
  import types._

  // def field[_: P] = P(modifiers ~ nonWildcardType ~ identifierText ~ ("=" ~/ expr).? ~ ";").map {
  //   case (mods, typ, name, initExpr) => new Field(name, mods.to(ArrayBuffer), typ, initExpr)
  // }

  def methodOrField[_: P]: P[Seq[Modifier] => Any] =
    P(returnType ~ identifierText ~/ (field2 | methodWithoutProofsOrTypeParams)).map {
      case (typ, text, fieldOrMethod) => fieldOrMethod(typ, text)
    }

  def field2[_: P]: P[(Type, Text) => Seq[Modifier] => Any] =
    P(("=" ~/ expr).? ~ ";").map { initExpr =>
      (typ: Type, name: Text) => (modifiers: Seq[Modifier]) => new Field(name, modifiers.to(ArrayBuffer), typ, initExpr)
    }

  //field or method
  // def templateDefMember[_: P]: P[Any] = P((normMethod: P[Any]) | (ctor: P[Any]) | (field: P[Any]))

  def templateDefMember2[_: P]: P[Any] =
    P(modifiers ~ (methodWithTypeParams | ctor | methodOrField | methodWithProofsWithoutTypeParams : P[Seq[Modifier] => Any])).map { 
      case (mods, astCtor) => astCtor(mods)
    }

  def classOrInterfaceBody[_: P]: P[(Int, Seq[Any], Int)] = P("{" ~/ Index ~ templateDefMember2.rep ~ "}" ~ Index)

  //TODO add modifiers and annotations
  //TODO allow extending classes, interfaces
  def clazz[_: P]: P[Seq[Modifier] => Classlike] = P(
      "class" ~/ Index ~ identifier ~ typeParamList.? ~
        ("extends" ~/ typeRef).? ~ //("implements" ~/ typeRef.rep).? ~
        classOrInterfaceBody
  ).map {
    case (classTokEnd, name, typeParams, superClass, /*interfaces, */(braceStart, members, braceEnd)) =>
      modifiers => {
        val (fields, normMethodsAndCtors) = members.partition(_.isInstanceOf[Field])
        val (normMethods, ctors) = normMethodsAndCtors.partition(_.isInstanceOf[NormMethod])
        /*println(s"superinterfaces=$interfaces")

        val is = interfaces.getOrElse(Nil)
        val it = is.iterator
        val isArr = Array.fill(is.size)(it.next(): Type)*/
        
        lazy val cls: ClassDef = new ClassDef(
            ArrayBuffer(), //todo annotations
            modifiers.to(ArrayBuffer),
            name,
            typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
            superClass.getOrElse(BuiltinTypes.objectType),
            Array.empty[Type],//isArr,
            fields.to(ArrayBuffer).asInstanceOf[ArrayBuffer[Field]],
            ctors.map(_.asInstanceOf[(() => Classlike) => Constructor](() => cls)).to(ArrayBuffer),
            normMethods.to(ArrayBuffer).asInstanceOf[ArrayBuffer[NormMethod]],
            ps2tr(classTokEnd - 5, classTokEnd),
            ps2tr(braceStart, braceEnd)
        )

        cls
      }
  }

  //TODO add modifiers and annotations
  //TODO allow extending classes, interfaces
  def interface[_: P]: P[Seq[Modifier] => Classlike] = P(
      "interface" ~/ Index ~ identifier ~ typeParamList.? ~ ("extends" ~/ typeRef ~ ("," ~/ typeRef).rep).? ~ classOrInterfaceBody
  ).map {
    case (classTokEnd, name, typeParams, superTypes, (braceStart, members, braceEnd)) =>
      modifiers => {
        val (fields, methods) = members.partition(_.isInstanceOf[Field])
        val castMethods = methods.asInstanceOf[Seq[NormMethod]]
        castMethods.foreach { mthd =>
          mthd.modifiers += Modifier(ModifierType.ABSTRACT, TextRange.synthetic)
        }
        new InterfaceDef(
            ArrayBuffer(), //todo annotations
            modifiers.to(ArrayBuffer),
            name,
            typeParams.getOrElse(TypeParamList(Seq.empty, TextRange.synthetic)),
            superTypes.fold(Nil: Seq[Type]){ case (first, rest) => first +: rest },
            fields.to(ArrayBuffer).asInstanceOf[ArrayBuffer[Field]],
            castMethods.to(ArrayBuffer),
            ps2tr(classTokEnd - 5, classTokEnd),
            ps2tr(braceStart, braceEnd)
        )
      }
  }

  //todo enums, interfaces, annotation definitions
  def classlike[_: P]: P[Classlike] = P(modifiers ~ (clazz | interface)).map {
    case (modifiers, buildClasslike) => buildClasslike(modifiers)
  }

}
