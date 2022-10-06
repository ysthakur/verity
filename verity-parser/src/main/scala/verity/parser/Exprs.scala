package verity.parser

import verity.ast.*

import verity.parser.Core.*
import verity.parser.Types.*
import verity.parser.VerityParser.tr

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.digit

/**
  * Parsers for expressions
  */
object Exprs {
  val boolLiteral: P[BoolLiteral] =
    ((identifier("true").as(true) | identifier("false").as(false)) ~ P.index)
      .map { case (value -> end) =>
        val text = if (value) "true" else "false"
        BoolLiteral(value, tr(end - text.length, end))
      }

  // TODO merge int and float literals, and allow underscores
  val intLiteral: P[IntLiteral] =
    (digit.rep.string ~ P.index).map { case (num -> end) =>
      IntLiteral(num.toInt, tr(end - num.length, end))
    }

  val nullLiteral: P[NullLiteral] =
    (identifier("null") *> P.index).map { endInd =>
      NullLiteral(tr(endInd - "null".length, endInd))
    }

  val stringLiteral: P[StringLiteral] =
    withRange(
      ((P.char('\\') ~ P.anyChar) | (P.charWhere(_ != '"'))).rep.string
        .surroundedBy(P.char('"'))
    ).map { case (start, text, end) =>
      StringLiteral("\"" + text + "\"", tr(start, end))
    }

  val thisRef: P[Expr] =
    (P.index.with1 ~ (identifier("this") *> P.index)).map { case (start, end) =>
      ThisRef(tr(start, end))
    }

  val superRef: P[Expr] =
    (P.index.with1 ~ (identifier("super") *> P.index)).map {
      case (start, end) =>
        SuperRef(tr(start, end))
    }

  val literal: P[Expr] =
    intLiteral | boolLiteral | nullLiteral | stringLiteral

  val varRef: P[Expr] = identifierWithTextRange.map { case (name, textRange) =>
    UnresolvedIdentifier(name, textRange)
  }

  val parenExpr: P[Expr] =
    P.defer(withRange(expr.between(P.char('(') ~ ws, ws ~ P.char(')')))).map {
      case (start, expr, end) =>
        ParenExpr(expr, tr(start, end))
    }

  /** Something with higher precedence than `.` */
  val selectable: P[Expr] = P.defer(
    parenExpr | literal | thisRef | superRef | varRef
  )

  /** `foo.bar` */
  val propAccess: P[Expr] =
    P.defer(selectable ~ (P.char('.').surroundedBy(ws) *> identifier).rep).map {
      case (obj, props) =>
        props.foldLeft(obj) { (obj, prop) => PropAccess(obj, prop) }
    }

  /** Helper to make parsers for different kinds of argument lists */
  def argList(start: P0[Unit]): P[List[Expr]] =
    P.defer(
      P.char('(') *> start *> ws *> expr.repSep0(ws ~ P.char(',') ~ ws) <* P
        .char(')')
    )

  val normArgList: P[NormArgList] = argList(P.unit).map(NormArgList(_))

  val givenArgList = argList(identifier("given")).map(GivenArgList(_))

  val proofArgList = argList(identifier("proof")).map(ProofArgList(_))

  val methodCall: P[Expr] = P
    .defer(
      propAccess ~ (ws.with1 *> typeArgList.?.with1 ~ normArgList ~ (ws *> givenArgList).? ~ (ws *> proofArgList).?).rep0
    )
    .map { case (obj, argLists) =>
      argLists.foldLeft(obj) {
        case (obj, typeArgs -> normArgs -> givenArgs -> proofArgs) =>
          FnCall(obj, typeArgs, normArgs, givenArgs, proofArgs)
      }
    }

  val mulDivMod: P[Expr] = binOp(methodCall, List("*", "/", "%"))
  val addSub: P[Expr] = binOp(mulDivMod, List("+", "-"))
  val relational: P[Expr] = binOp(addSub, List("<=", "<", ">=", ">"))
  val eqNotEq: P[Expr] = binOp(relational, List("==", "!="))
  val logicAnd: P[Expr] = binOp(eqNotEq, List("&&"))
  val logicOr: P[Expr] = binOp(logicAnd, List("||"))

  /** Has to be right-associative */
  val assignment: P[Expr] = P.defer(logicOr.repSep(ws ~ P.string("<-") ~ ws)).map {
    case NonEmptyList(first, rest) => rest.foldRight(first) { (rvalue, lvalue) => AssignExpr(lvalue, rvalue) }
  }

  // TODO add if expressions
  val expr: P[Expr] = logicOr

  /** Helper to make a parser for a binary expression
    * @param prev
    *   The parser for the operator that has higher precedence than this one
    * @param ops
    *   The operators that the current parser should parse
    */
  def binOp(prev: P[Expr], ops: List[String]) =
    P.defer(
      prev ~ (withRange(P.stringIn(ops)).surroundedBy(ws) ~ prev).rep0
    ).map { case (left, reps) =>
      reps.foldLeft(left) { case (lhs, (opStart, op, opEnd) -> rhs) =>
        BinExpr(
          lhs,
          Op(op, tr(opStart, opEnd)),
          rhs
        )
      }
    }
}
