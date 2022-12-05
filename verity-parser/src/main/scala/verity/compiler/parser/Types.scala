package verity.compiler.parser

import verity.compiler.ast.*
import verity.compiler.parser.Core.*
import verity.compiler.parser.Parser.tr

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}

/** Parsers for type-related stuff
  */
object Types {

  /** TODO find a better name for this A type like `foo` or `foo.bar.baz` (only
    * names separated by dots)
    */
  val idsWithDots: P[Type] = (identifier <* ws).repSep(P.char('.') ~ ws).map {
    case (NonEmptyList(first, restPath)) => UnresolvedType(first +: restPath)
  }

  val parenType: P[Type] =
    P.defer(withRange(typ.between(P.char('(') ~ ws, ws ~ P.char(')')))).map {
      case (start, inner, end) => ParenType(inner, tr(start, end))
    }

  val selectableType: P[Type] = idsWithDots | parenType

  val typeArgList =
    (P.char('[') *> ws *>
      P.defer(typ).repSep0(ws ~ P.char(',') ~ ws)
      <* ws <* P.char(']')).map { case args => TypeArgList(args) }

  val typeApply: P[Type => Type] = typeArgList.rep.map { case argLists =>
    typ =>
      argLists.foldLeft(typ) { (typeCtor, argList) =>
        TypeApply(typeCtor, argList)
      }
  }

  val typeDotAccess: P[Type => Type] =
    identifierWithTextRange.between(P.char('.') ~ ws, ws).rep.map {
      case argLists =>
        typ =>
          argLists.foldLeft(typ) { case (typ, (memberName, memberRange)) =>
            TypeMemberAccess(typ, memberName)
          }
    }

  /** Type application (`foo[bar]`) and type member access (`foo.bar`). Put in
    * one parser because they have the same precedence.
    */
  val typeApplyOrDot: P[Type] =
    P.defer((selectableType <* ws.?) ~ (typeApply | typeDotAccess).rep0).map {
      case (firstType, dotsAndApplys) =>
        dotsAndApplys.foldLeft(firstType) { (typ, fn) => fn(typ) }
    }

  val wildcard: P[Type] = P.char('?').map { case _ =>
    Wildcard(UnknownType, UnknownType)
  }

  val typ: P[Type] = P.defer(wildcard | typeApplyOrDot)

  def typeParam: P[TypeParam] =
    (identifierWithTextRange ~ typ.between(ws ~ P.string("<:") ~ ws, ws).? ~ typ
      .between(ws ~ P.string(">:") ~ ws, ws)
      .?).map { case (name -> nameRange -> upperBound -> lowerBound) =>
      TypeParam(
        name,
        upperBound,
        lowerBound,
        nameRange
      )
    }

  def typeParamList: P[TypeParamList] =
    withRange(P.char('[') *> ws *> typeParam.repSep0(P.char(',')) <* ws <* P.char(']'))
      .map { case (start, params, end) => TypeParamList(params) }
}
