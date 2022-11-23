package verity.compiler.parser

import verity.compiler.ast.*

import verity.compiler.parser.Core.*
import verity.compiler.parser.Types.*
import verity.compiler.parser.Parser.tr

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.digit

/** Parsers for expressions
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
    P.defer(
      selectable ~ (P.char('.').surroundedBy(ws) *> identifier).backtrack.rep0
    ).map { case (obj, props) =>
      props.foldLeft(obj) { (obj, prop) => PropAccess(obj, prop) }
    }

  /** Helper to make parsers for different kinds of argument lists */
  def argList(start: P0[Unit]): P[List[Expr]] =
    P.defer(
      P.char('(') *> start *> ws *> expr.backtrack.repSep0(
        ws ~ P.char(',') ~ ws
      ) <* P.char(')')
    )

  val normArgList: P[NormArgList] = argList(P.unit).map(NormArgList(_))

  val givenArgList = argList(identifier("given")).map(GivenArgList(_))

  val proofArgList = argList(identifier("proof")).map(ProofArgList(_))

  val methodCall: P[Expr] = P
    .defer(
      (propAccess <* ws) ~ (typeArgList.?.with1 ~ normArgList ~ (ws *> givenArgList).? ~ (ws *> proofArgList).?).backtrack.rep0
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
  val assignment: P[Expr] =
    P.defer(logicOr.repSep(ws ~ P.string("<-") ~ ws)).map {
      case NonEmptyList(first, rest) =>
        rest.foldRight(first) { (rvalue, lvalue) => AssignExpr(lvalue, rvalue) }
    }

  val varDef: P[VarDef] =
    (
      identifier.surroundedBy(ws)
        ~ (P.char(':') *> ws *> typ <* ws).?
        ~ (P.char('=') *> ws *> assignment <* ws)
    ).map { case (varName -> typ -> value) =>
      VarDef(varName, typ, value)
    }

  val varDefs = identifier("val") *> ws *> varDef.repSep(P.char(',') ~ ws)

  val varDefExpr: P[Expr] =
    P.defer((varDefs <* ws <* identifier("in") <* ws).rep0.with1 ~ assignment).map {
      case (varLists, body) =>
        varLists.foldRight(body) {
          case (NonEmptyList(firstVar, rest), body) => LetExpr(firstVar :: rest, body)
        }
    }

  // TODO add if expressions
  val expr: P[Expr] = varDefExpr

  /** Helper to make a parser for a binary expression
    * @param prev
    *   The parser for the operator that has higher precedence than this one
    * @param ops
    *   The operators that the current parser should parse
    */
  def binOp(prev: P[Expr], ops: List[String]) =
    // todo get rid of the backtrack? shouldn't be necessary
    // maybe .soft can be used instead
    P.defer(
      prev ~ ((P.stringIn(ops) ~ P.index)
        .surroundedBy(ws) ~ prev).backtrack.rep0
    ).map { case (left, reps) =>
      reps.foldLeft(left) { case (lhs, (op, opEnd) -> rhs) =>
        BinExpr(
          lhs,
          Op(op, tr(opEnd - op.length, opEnd)),
          rhs
        )
      }
    }

  private val param: P[ValParam] =
    ((identifier <* ws <* P.char(':') <* ws) ~ typ).map { case (name, typ) =>
      ValParam(name, typ)
    }

  /** Helper to make parsers for different kinds of parameter lists */
  def paramList(start: P0[Unit], kind: ParamListKind): P[ValParamList] =
    P.defer(
      P.char('(') *> ws *> start *> ws *>
        param.repSep0(ws ~ P.char(',') ~ ws) <* ws <* P.char(')')
    ).map { case params =>
      ValParamList(params, kind)
    }

  val normParamList = paramList(P.unit, ParamListKind.Normal)

  val givenParamList = paramList(identifier("given"), ParamListKind.Given)

  val proofParamList = paramList(identifier("proof"), ParamListKind.Proof)

  // val provenClause: P[Seq[Type]] =
  //   (
  //     identifier("proof") ~~ !CharPred(
  //       _.isUnicodeIdentifierPart
  //     ) ~/ typeRef ~ ("," ~ typeRef).rep
  //   ).map { case (firstType, restTypes) =>
  //     firstType +: restTypes
  //   }

  def lambda: P[Lambda] =
    (P.char('\\') *> P.index
      ~ (typeParamList.? <* ws)
      ~ (normParamList.? <* ws)
      ~ (givenParamList.? <* ws)
      ~ (proofParamList.? <* ws)
      ~ (P.string("->") *> expr)
      ~ P.index).map {
      case (start -> typeParams -> normParams -> givenParams -> proofParams -> body -> end) =>
        Lambda(
          typeParams,
          normParams,
          givenParams,
          proofParams,
          body,
          tr(start, end)
        )
    }
}
