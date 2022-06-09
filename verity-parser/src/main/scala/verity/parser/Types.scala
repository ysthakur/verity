package verity.parser

import verity.ast._

// import verity.parser.Core.{argList, identifierText, identifierWithTextRange}
import Parser.ps2tr

import fastparse.JavaWhitespace._
import fastparse._

private class Types(core: Core)(implicit
  offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]
) {
  import core._

  // todo clear this up?
  def upperBound[_: Parser]: Parser[UnresolvedTypeRef] = Parser("extends" ~/ typeRef)
  def lowerBound[_: Parser]: Parser[UnresolvedTypeRef] = Parser("super" ~/ typeRef)
  // def typeBound[_: Parser] = Parser(Index ~ StringIn("super", "extends").! ~/ Index ~ typeRef)

  def typeRef[_: Parser]: Parser[UnresolvedTypeRef] =
    Parser(identifierText ~ ("." ~ identifierText).rep ~ typeArgList).map {
      case (first, restPath, args) =>
        UnresolvedTypeRef(
          first +: restPath,
          args,
          None
        )
    }
  def wildCard[_: Parser]: Parser[UnresolvedWildcard] =
    Parser("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map { case (upper, lower) =>
      UnresolvedWildcard(upper, lower)
    }
  def primitiveType[_: Parser]: Parser[PrimitiveType] =
    Parser(StringIn("boolean", "byte", "char", "short", "int", "float", "long", "double").! ~ Index)
      .map { case (typ, end) =>
        PrimitiveType(PrimitiveTypeDef.fromName(typ).get, ps2tr(end - typ.length, end))
      }

  /** A type that isn't just a wildcard, possibly an array type
    */
  def nonWildcardType[_: Parser]: Parser[Type] =
    Parser((primitiveType | typeRef) ~ ("[" ~ Index ~ "]" ~ Index).rep).map {
      case (innerType, brackets) =>
        brackets.foldLeft(innerType: Type) { case (typ, (start, end)) =>
          ArrayType(typ, ps2tr(start, end))
        }
    }
  def typeArg[_: Parser]: Parser[Type] = Parser(wildCard | nonWildcardType)
  def typeArgList[_: Parser]: Parser[TypeArgList] =
    Parser(("[" ~/ Index ~ typeArg ~ ("," ~ typeArg).rep ~ "]" ~ Index).?).map {
      case Some((start, firstArg, restArgs, end)) =>
        // println(s"typearglist, ${firstArg +: restArgs}")
        TypeArgList(firstArg +: restArgs, ps2tr(start, end))
      case None => TypeArgList(Nil, TextRange.synthetic)
    }

  def returnType[_: Parser]: Parser[Type] =
    Parser(("void" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ Index) | nonWildcardType).map { t =>
      (t: @unchecked) match {
        case end: Int => new VoidTypeRef(ps2tr(end - 4, end))
        case typ: Type => typ
      }
    }

  def typeParam[_: Parser]: Parser[TypeParam] =
    Parser(identifierWithTextRange ~ upperBound.? ~ lowerBound.?).map {
      case (name, nameRange, upper, lower) =>
        new TypeParam(
          name,
          upper.getOrElse(BuiltinTypes.objectType),
          lower.getOrElse(NothingTypeDef.makeRef),
          nameRange
        )
    }
  def typeParamList[_: Parser]: Parser[TypeParamList] =
    Parser("[" ~/ Index ~ typeParam ~ ("," ~ typeParam).rep ~ "]" ~ Index).map {
      case (start, first, rest, end) => new TypeParamList(first +: rest, ps2tr(start, end))
    }

}
