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

  val stringLiteral: P[StringLiteral] =
    withRange(
      ((P.char('\\') ~ P.anyChar) | (P.charWhere(_ != '"'))).rep.string
        .surroundedBy(P.char('"'))
    ).map { case (start, text, end) =>
      StringLiteral("\"" + text + "\"", tr(start, end))
    }

  val literal: P[Expr] =
    intLiteral | boolLiteral | stringLiteral

  val varRef: P[Expr] = identifierWithTextRange.map { case (name, textRange) =>
    UnresolvedIdentifier(name, textRange)
  }

  val parenExpr: P[Expr] =
    P.defer(withRange(expr.between(P.char('(') ~ ws, ws ~ P.char(')')))).map {
      case (start, expr, end) =>
        ParenExpr(expr, tr(start, end))
    }

  /** Something with higher precedence than `.` */
  val selectable: P[Expr] = parenExpr | literal | varRef

  /** Helper to make parsers for different kinds of argument lists */
  def argList(start: P0[Unit]): P[List[Expr]] =
    P.defer(
      P.char('(') *> start *> ws *> expr.backtrack.repSep0(
        ws ~ P.char(',') ~ ws
      ) <* P.char(')')
    )

  val fieldAccess: P[String] = P.char('.') *> ws *> identifier

  /** Either `foo.bar` or `foo[T](bar)(given baz)` */
  val fnCallOrFieldAccess: P[Expr] =
    (selectable ~ fieldAccess
      .eitherOr(valArgList.eitherOr(constArgList))
      .repSep0(ws)).map { case (start, rest) =>
      rest.foldLeft(start) { (expr, next) =>
        next match {
          case Right(fieldName)         => FieldAccess(expr, fieldName)
          case Left(Right(valArgList))  => ???
          case Left(Left(constArgList)) => ???
        }
      }
    }

  val binop8 = binOp(fnCallOrFieldAccess, op('?', '@', '~', '\\', '$'))
  val binop7 = binOp(binop8, op('*', '/', '%'))
  val binop6 = binOp(binop7, op('+', '-'))
  val binop5 = binOp(binop6, op('<', '>'))
  val binop4 = binOp(binop5, op('!') | (P.char('=') ~ ((P.char('=').as('=') ~ opChar).string | opChar.string)).rep.string)
  val binop3 = binOp(binop4, op('&'))
  val binop2 = binOp(binop3, op('^'))
  val binop1 = binOp(binop2, op('|'))

  /** Has to be right-associative */
  val assignment: P[Expr] =
    P.defer(binop1.repSep(ws ~ P.string("=") ~ ws)).map {
      case NonEmptyList(first, rest) =>
        rest.foldRight(first) { (rvalue, lvalue) => AssignExpr(lvalue, rvalue) }
    }

  val ifExpr: P[Expr] =
    ((identifier("if") *> ws *> expr)
      ~ (identifier("then").surroundedBy(ws) *> expr)
      ~ (identifier("else").surroundedBy(ws) *> expr)).map {
      case (cond -> thenBody -> elseBody) => If(cond, thenBody, elseBody)
    }

  val varDef: P[VarDef] =
    (
      identifier("let") *> identifier.surroundedBy(ws)
        ~ (P.char(':') *> ws *> typ <* ws).?
        ~ (P.char('=') *> ws *> assignment <* ws)
    ).map { case (varName -> typ -> value) =>
      VarDef(varName, typ, value)
    }

  val letExpr: P[Expr] =
    P.defer((varDef.repSep(ws) <* ws <* identifier("in") <* ws).rep0.with1 ~ expr)
      .map { case (varLists, body) =>
        varLists.foldRight(body) { case (NonEmptyList(firstVar, rest), body) =>
          LetExpr(firstVar :: rest, body)
        }
      }

  val valParam: P[ValParam] =
    (identifier ~ (ws *> P.char(':') *> ws *> typ).?).map { case (name, typ) =>
      ValParam(name, typ.getOrElse(UnknownType))
    }
  val normParamList: P[ValParamList] =
    list(P.char('('), valParam, P.char(','), P.char(')')).map {
      ValParamList(_, false)
    }
  val givenParamList: P[ValParamList] =
    list(P.char('('), valParam, P.char(','), P.char(')')).map {
      ValParamList(_, true)
    }
  val valParamList: P[ValParamList] = normParamList | givenParamList

  val typeParamList: P[ConstParamList] =
    list(P.char('['), typeParam, P.char(','), P.char(']')).map {
      ConstParamList.TypeParamList(_)
    }
  val proofParamList: P[ConstParamList] =
    list(P.char('{'), valParam, P.char(','), P.char('}')).map {
      ConstParamList.ProofParamList(_)
    }
  val constParamList: P[ConstParamList] = typeParamList | proofParamList

  val typeArgList: P[ConstArgList] =
    list(P.char('['), typ, P.char(','), P.char(']')).map {
      ConstArgList.TypeArgList(_)
    }
  val proofArgList: P[ConstArgList] =
    list(P.char('{'), expr, P.char(','), P.char('}')).map {
      ConstArgList.ProofArgList(_)
    }
  val constArgList: P[ConstArgList] = typeArgList | proofArgList

  val normArgList: P[ValArgList] = argList(P.unit).map(ValArgList(_, false))
  val givenArgList: P[ValArgList] =
    argList(identifier("given")).map(ValArgList(_, true))
  val valArgList: P[ValArgList] = normArgList | givenArgList

  val lambda: P[Expr] =
    (P.char('\\') *> P.index
      ~ constParamList.repSep0(ws)
      ~ valParamList.repSep0(ws)
      ~ (P.string("->") *> expr)
      ~ P.index).map {
      case (start -> constParamss -> valParamss -> body -> end) =>
        Lambda(
          constParamss,
          valParamss,
          body,
          tr(start, end)
        )
    }

  // TODO add if expressions
  val expr: P[Expr] = lambda | letExpr | ifExpr | assignment

  /** A character that can be part of an operator */
  def opChar: P[Char] = P.charIn("~!@$%^&*-+=:<>/?|\\")

  def op(starts: Char*): P[String] =
    (P.charIn(starts) ~ opChar.rep0).string

  /** Helper to make a parser for a binary expression
    * @param prev
    *   The parser for the operator that has higher precedence than this one
    * @param op
    *   Parser for the operator
    */
  def binOp(prev: P[Expr], op: P[String]) =
    (prev ~ ((op ~ P.index <* ws) ~ prev).repSep0(ws))
      .map { case (left, reps) =>
        reps.foldLeft(left) { case (lhs, op -> opEnd -> rhs) =>
          BinExpr(
            lhs,
            Op(op, tr(opEnd - op.length, opEnd)),
            rhs
          )
        }
      }
}
