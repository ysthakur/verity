package verity.parser

import verity.ast._

// import verity.parser.Core.{argList, identifierText, identifierWithTextRange}
import VerityParser.ps2tr

import cats.parse.{Parser as P, Parser0 as P0}

private class Types(core: Core)(implicit
  offsetToPos: collection.mutable.ArrayBuffer[(Int, Int, Int)]
) {
  import core._

  // todo clear this up?
  def upperBound: P[UnresolvedTypeRef] = P("extends" ~/ typeRef)
  def lowerBound: P[UnresolvedTypeRef] = P("super" ~/ typeRef)
  // def typeBound = P(P.index ~ StringIn("super", "extends").! ~/ P.index ~ typeRef)

  def typeRef: P[UnresolvedTypeRef] =
    P(identifierText ~ ("." ~ identifierText).rep ~ typeArgList).map {
      case (first, restPath, args) =>
        UnresolvedTypeRef(
          first +: restPath,
          args,
          None
        )
    }
  def wildCard: P[UnresolvedWildcard] =
    P("?" ~ ("extends" ~ typeRef).? ~ ("super" ~ typeRef).?).map { case (upper, lower) =>
      UnresolvedWildcard(upper, lower)
    }
  def primitiveType: P[PrimitiveType] =
    P(StringIn("boolean", "byte", "char", "short", "int", "float", "long", "double").! ~ P.index)
      .map { case (typ, end) =>
        PrimitiveType(PrimitiveTypeDef.fromName(typ).get, ps2tr(end - typ.length, end))
      }

  /** A type that isn't just a wildcard, possibly an array type
    */
  def nonWildcardType: P[Type] =
    P((primitiveType | typeRef) ~ ("[" ~ P.index ~ "]" ~ P.index).rep).map {
      case (innerType, brackets) =>
        brackets.foldLeft(innerType: Type) { case (typ, (start, end)) =>
          ArrayType(typ, ps2tr(start, end))
        }
    }
  def typeArg: P[Type] = P(wildCard | nonWildcardType)
  def typeArgList: P[TypeArgList] =
    P(("[" ~/ P.index ~ typeArg ~ ("," ~ typeArg).rep ~ "]" ~ P.index).?).map {
      case Some((start, firstArg, restArgs, end)) =>
        // println(s"typearglist, ${firstArg +: restArgs}")
        TypeArgList(firstArg +: restArgs, ps2tr(start, end))
      case None => TypeArgList(Nil, TextRange.synthetic)
    }

  def returnType: P[Type] =
    P(("void" ~~ !CharPred(_.isUnicodeIdentifierPart) ~/ P.index) | nonWildcardType).map { t =>
      (t: @unchecked) match {
        case end: Int => new VoidTypeRef(ps2tr(end - 4, end))
        case typ: Type => typ
      }
    }

  def typeParam: P[TypeParam] =
    P(identifierWithTextRange ~ upperBound.? ~ lowerBound.?).map {
      case (name, nameRange, upper, lower) =>
        new TypeParam(
          name,
          upper.getOrElse(BuiltinTypes.objectType),
          lower.getOrElse(NothingTypeDef.makeRef),
          nameRange
        )
    }

  def typeParamList: P[TypeParamList] =
    (P.index ~ typeParam.repSep(P.char(',')).between(P.char('['), P.char(']')) ~ P.index).map {
      case (start, NonEmptyList(first, last), end) => new TypeParamList(first +: rest, ps2tr(start, end))
    }
}
