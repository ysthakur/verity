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

  val typ: P[Type] = P.recursive { typ =>
    val parenType: P[Type] =
      P.defer(withRange(typ.between(P.char('(') ~ ws, ws ~ P.char(')')))).map {
        case (start, inner, end) => ParenType(inner, tr(start, end))
      }

    val selectableType: P[Type] = idsWithDots | parenType

    val typeArgList: P[List[Type]] =
      list(P.char('['), P.defer(typ), P.char(','), P.char(']'))

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

    typeApplyOrDot
  }

  // todo allow adding kind
  def typeParam: P[TypeParam] =
    (identifierWithTextRange).map { case (name, nameRange) =>
      TypeParam(
        name,
        nameRange
      )
    }
}
