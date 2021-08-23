package verity.parser

import verity.ast.*
import infile.*
// import Core._
// import Exprs._
// import Methods._
// import Types._
import Parser.ps2tr
import fastparse.*
import JavaWhitespace.*

import collection.mutable.ArrayBuffer

//TODO add annotations
private class Classlikes(core: Core, types: Types, exprs: Exprs, methods: Methods)(implicit
  offsetToPos: ArrayBuffer[(Int, Int, Int)]
) {
  import core.*
  import exprs.*
  import methods.*
  import types.*

  /** Matches a field without its modifiers, then returns a function that takes
    * those modifiers and creates an actual Field object.
    */
  def field[_: P]: P[Seq[Modifier] => Any] =
    P(valOrVal ~/ identifierText ~ ":" ~ typeArg ~ ("=" ~/ expr).? ~ ";").map {
      case (isFinal, name, typ, initExpr) =>
        (modifiers: Seq[Modifier]) =>
          new Field(name, modifiers.to(ArrayBuffer), typ, initExpr, isFinal)
    }

  /** Matches a bunch of modifiers, then tries to match the method, field, or constructor
    * that those modifiers belong to.
    */
  def templateDefMember[_: P]: P[Any] =
    P(modifiers ~ (normMethod | field | ctor: P[Seq[Modifier] => Any])).map {
      case (mods, astCtor) => astCtor(mods)
    }

  def classOrInterfaceBody[_: P]: P[(Int, Seq[Any], Int)] = P(
    "{" ~/ Index ~ templateDefMember.rep ~ "}" ~ Index
  )

  //TODO Add enum cases
  // def enumBody[_: P]: P[(Int, Seq[Any], Int)] = P("{" ~/ Index ~ templateDefMember.rep ~ "}" ~ Index)

  //TODO add annotations
  //TODO allow extending classes, interfaces
  def clazz[_: P]: P[Seq[Modifier] => Classlike] = P(
    "class" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ Index
      ~ identifier ~ typeParamList.?
      ~ ("extends" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ typeRef).?
      ~ ("implements" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ typeRef ~ ("," ~ typeRef).rep).?
      ~ classOrInterfaceBody
  ).map {
    case (
          classTokEnd,
          name,
          typeParams,
          superClass, superInterfaces, (braceStart, members, braceEnd)
        ) =>
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
          superInterfaces.fold(Array.empty[Type]) { case (first, rest) => (first +: rest).toArray },
          //isArr,
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
  ).map { case (classTokEnd, name, typeParams, superTypes, (braceStart, members, braceEnd)) =>
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
        superTypes.fold(Nil: Seq[Type]) { case (first, rest) => first +: rest },
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
