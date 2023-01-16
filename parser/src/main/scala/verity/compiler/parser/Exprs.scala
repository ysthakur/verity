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
    ((keyword("true").as(true) | keyword("false").as(false)) ~ P.index)
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
      .eitherOr(P.char('a') /*valArgs.eitherOr(comptimeArgs ~~ valArgs)*/)
      .repSep0(ws)).map { case (start, rest) =>
      rest.foldLeft(start) { (expr, next) =>
        next match {
          case Right(fieldName)     => FieldAccess(expr, fieldName)
          case _ => ???
          // case Left(Right(valArgs)) => FnCall(expr, ComptimeArgs.empty, valArgs)
          // case Left(Left( /*comptimeArgs ->*/ valArgs)) =>
          //   FnCall(expr, ComptimeArgs.empty /*comptimeArgs*/, valArgs)
        }
      }
    }

  val binop8 = binOp(fnCallOrFieldAccess, op('?', '@', '~', '\\', '$'))
  val binop7 = binOp(binop8, op('*', '/', '%'))
  val binop6 = binOp(binop7, op('+', '-'))
  val binop5 = binOp(binop6, op('<', '>'))
  val binop4 = binOp(
    binop5,
    op('!') | (P.char('=') ~ ((P
      .char('=')
      .as('=') ~ opChar).string | opChar.string)).rep.string
  )
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
    ((keyword("if") *> ws *> expr)
      ~ (keyword("then").surroundedBy(ws) *> expr)
      ~ (keyword("else").surroundedBy(ws) *> expr)).map {
      case (cond -> thenBody -> elseBody) => If(cond, thenBody, elseBody)
    }

  val varDef: P[VarDef] =
    (
      keyword("let") *> identifier.surroundedBy(ws)
        ~ (P.char(':') *> ws *> typ <* ws).?
        ~ (P.char('=') *> ws *> assignment <* ws)
    ).map { case (varName -> typ -> value) =>
      VarDef(varName, typ, value)
    }

  val letExpr: P[Expr] =
    P.defer(
      (varDef.repSep(ws) <* ws <* keyword("in") <* ws).rep0.with1 ~ expr
    ).map { case (varLists, body) =>
      varLists.foldRight(body) { case (NonEmptyList(firstVar, rest), body) =>
        LetExpr(firstVar :: rest, body)
      }
    }

  val valParam: P[Param] =
    (identifier ~ (ws *> P.char(':') *> ws *> typ).?).map { case (name, typ) =>
      Param(name, typ.getOrElse(UnknownType))
    }
  val normParamList: P[List[Param]] =
    list(P.char('('), valParam, P.char(','), P.char(')'))
  val givenParamList: P[List[Param]] =
    list(
      P.char('(') *> ws *> keyword("given"),
      valParam,
      P.char(','),
      P.char(')')
    )

  /** A normal parameter list and an implicit parameter list together */
  val params: P[Params] = givenParamList
    .map(Params(Nil, _))
    .orElse((normParamList ~ givenParamList.?).map {
      case (normParams, givenParams) =>
        Params(normParams, givenParams.getOrElse(Nil))
    })
  val params0: P0[Params] =
    ((normParamList <* ws).? ~ givenParamList.?).map {
      case (normParams, givenParams) =>
        Params(normParams.getOrElse(Nil), givenParams.getOrElse(Nil))
    }

  val typeParamList: P[List[TypeParam]] =
    list(P.char('['), typeParam, P.char(','), P.char(']'))
  val normConstParamList: P[List[Param]] =
    list(P.char('{'), valParam, P.char(','), P.char('}'))
  val givenConstParamList: P[List[Param]] =
    list(
      P.char('{') *> ws *> keyword("given"),
      valParam,
      P.char(','),
      P.char('}')
    )
  val comptimeParams: P[ComptimeParams] = {
    // Has type params
    val typeParamsFirst =
      (typeParamList ~ normConstParamList.? ~ givenConstParamList.?).map {
        case (typeParams -> normConstParams -> givenConstParams) =>
          ComptimeParams(
            typeParams,
            normConstParams.getOrElse(Nil),
            givenConstParams.getOrElse(Nil)
          )
      }
    // No type params
    val normConstFirst = (normConstParamList ~ givenConstParamList.?).map {
      case (normConstParams -> givenConstParams) =>
        ComptimeParams(Nil, normConstParams, givenConstParams.getOrElse(Nil))
    }
    // No type params and no normal const params, only given const params
    val givenConstOnly = givenConstParamList.map { givenConstParams =>
      ComptimeParams(Nil, Nil, givenConstParams)
    }

    typeParamsFirst | normConstFirst | givenConstOnly
  }
  val comptimeParams0: P0[ComptimeParams] =
    (typeParamList.? ~~ normConstParamList.? ~~ givenConstParamList.?).map {
      case (typeParams -> normConstParams -> givenConstParams) =>
        ComptimeParams(
          typeParams.getOrElse(Nil),
          normConstParams.getOrElse(Nil),
          givenConstParams.getOrElse(Nil)
        )
    }

  val typeArgList: P[List[Type]] =
    list(P.char('['), typ, P.char(','), P.char(']'))
  val normConstArgList: P[List[Expr]] =
    list(P.char('{'), expr, P.char(','), P.char('}'))
  val givenConstArgList: P[List[Expr]] =
    list(
      P.char('{') *> ws *> keyword("given"),
      expr,
      P.char(','),
      P.char('}')
    )
  val comptimeArgs: P[ComptimeArgs] = {
    // Has type args
    val typeArgsFirst =
      (typeArgList ~~ normConstArgList.? ~~ givenConstArgList.?).map {
        case (typeArgs -> normConstArgs -> givenConstArgs) =>
          ComptimeArgs(
            typeArgs,
            normConstArgs.getOrElse(Nil),
            givenConstArgs.getOrElse(Nil)
          )
      }
    // No type args
    val normConstFirst = (normConstArgList ~~ givenConstArgList.?).map {
      case (normConstArgs -> givenConstArgs) =>
        ComptimeArgs(Nil, normConstArgs, givenConstArgs.getOrElse(Nil))
    }
    // No type args and no normal const args, only given const args
    val givenConstOnly = givenConstArgList.map { givenConstArgs =>
      ComptimeArgs(Nil, Nil, givenConstArgs)
    }
    typeArgsFirst | normConstFirst | givenConstOnly
  }

  val normArgList: P[List[Expr]] =
    list(P.char('('), expr, P.char(','), P.char(')'))
  val givenArgList: P[List[Expr]] = list(
    P.char('(') *> ws *> keyword("given"),
    expr,
    P.char(','),
    P.char(')')
  )
  val valArgs: P[Args] = givenArgList
    .map(Args(Nil, _))
    .orElse((normArgList ~ givenArgList.?).map { case (normArgs, givenArgs) =>
      Args(normArgs, givenArgs.getOrElse(Nil))
    })

  val lambda: P[Expr] =
    (P.char('\\') *> P.index
      ~ comptimeParams.surroundedBy(ws)
      ~ params
      ~ (ws *> P.string("->") *> expr)
      ~ P.index).map {
      case (start -> comptimeParams -> params -> body -> end) =>
        Lambda(
          comptimeParams,
          params,
          body,
          tr(start, end)
        )
    }

  // TODO add if expressions
  val expr: P[Expr] = (lambda | letExpr | ifExpr | assignment).nn

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
    (prev ~ ((op ~ P.index <* ws) ~ prev).repSep0(ws)).map {
      case (left, reps) =>
        reps.foldLeft(left) { case (lhs, op -> opEnd -> rhs) =>
          BinExpr(
            lhs,
            Op(op, tr(opEnd - op.length, opEnd)),
            rhs
          )
        }
    }
}
