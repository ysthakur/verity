package verity.parsing.parser

import language.implicitConversions

import verity.ast._, infile._
import Core._

import fastparse._, JavaWhitespace._

private object Exprs {
  def boolLiteral[_: P] = P(Index ~ ("true".! | "false".!) ~ Index).map { case (start, text, end) =>
    if (text(0) == 't') new BoolLiteral.TrueLiteral(TextRange(start, end))
    else new BoolLiteral.FalseLiteral(TextRange(start, end))
  }
  //TODO merge int and float literals, and allow underscores
  def intLiteral[_: P] = P(Index ~ CharsWhileIn("0-9").! ~ CharIn("lL").?.! ~ Index).map {
    case (start, num, l, end) => new IntegerLiteral(num, TextRange(start, end), l == "L")
  }
  def numLiteral[_: P] = P(intLiteral)
  def stringLiteral[_: P] =
    P("\"" ~/ Index ~ (("\\" ~/ AnyChar) | (!"\"" ~ AnyChar)).rep.! ~ "\"" ~ Index).map {
      case (start, text, end) => new StringLiteral("\"" + text + "\"", TextRange(start, end))
    }
  def thisRef[_: P] = P(Index ~ "this" ~ Index).map { case (start, end) =>
    new ThisRef(TextRange(start, end))
  }
  def superRef[_: P] = P(Index ~ "super" ~ Index).map { case (start, end) =>
    new SuperRef(TextRange(start, end))
  }
  def literal[_: P] = P(numLiteral | boolLiteral | stringLiteral | thisRef | superRef)
  // def varRef[_: P] = P(Index ~ identifier ~ Index).map { case (start, varName, end) =>
  //   new VarRef(varName, TextRange(start, end))
  // }
  def parenExpr[_: P] = P(Index ~ "(" ~ expr ~/ ")" ~ Index).map { case (start, expr, end) =>
    new ParenExpr(expr, TextRange(start, end))
  }
  //TODO
  def varRefOrMethodCall[_: P]: P[Expr] =
    P(Index ~ identifier ~ ("." ~ identifierWithTextRange) ~ ("(" ~/ Index ~ methodArgList ~ ")").? ~ Index).map {
      case (start, first, rest, Some((argStart, args, argsEnd)), end) => new MethodCall(None, name, args, Nil)
      case (start, first, rest, _, end)                               => new VarRef(name, TextRange(start, end))
    }

  def methodArg[_: P] = P(expr).map(Argument.apply)
  def methodArgList[_: P] =
    P("(" ~/ Index ~ (methodArg ~ ("," ~ methodArg).rep).? ~ ")" ~ Index).map {
      case (start, None, end) => ArgList(Nil, TextRange(start, end))
      case (start, Some((firstArg, args)), end) =>
        ArgList(firstArg :: args.toList, TextRange(start, end))
    }

  def selectable[_: P]: P[Expr] = P(parenExpr | literal | varRefOrMethodCall)

  def fieldAccessOrMethodCall[_: P]: P[Expr => Expr] = P("." ~ identifier ~ methodArgList.?).map {
    case (name, Some(args)) =>
      (obj: Expr) => MethodCall(Some(obj), name, args, Nil)
    case (name, _) => (obj: Expr) => FieldAccess(obj, name)
  }
  def typeParamsFirstMethodCall[_: P]: P[Expr => Expr] = P(
      "." ~ "<" ~/ Index ~ typeArgList ~ ">" ~ Index ~ identifier ~ methodArgList
  ).map { case (typeArgStart, typeArgs, typeArgEnd, name, valArgs /*, givenArgs*/ ) =>
    (obj: Expr) => MethodCall(Some(obj), name, valArgs, /*givenArgs,*/ typeArgs)
  }
  def arrayAccess[_: P]: P[Expr => Expr] = P("[" ~/ Index ~ expr ~ "]" ~ Index).map {
    case (start, index, end) => (arr: Expr) => ArraySelect(arr, index, TextRange(start, end))
  }

  def topExpr[_: P]: P[Expr] =
    P(selectable ~ (fieldAccessOrMethodCall | typeParamsFirstMethodCall | arrayAccess).rep).map {
      case (expr, ctors) => ctors.foldLeft(expr)((e, f) => f(e))
    }

  def mulDivMod[_: P] = P(topExpr ~ (CharIn("*/").! ~/ topExpr).rep).map(foldBinExpr)
  // def mulDivMod[_: P] = binExpr(CharIn("*/%").!, topExpr)
  def addSub[_: P] = P(mulDivMod ~ (CharIn("+\\-").! ~/ mulDivMod).rep).map(foldBinExpr)
  def bitShift[_: P] = P(addSub ~ (StringIn(">>>", ">>", "<<").! ~/ addSub).rep).map(foldBinExpr)
  def relational[_: P] =
    P(bitShift ~ (StringIn("<=", "<", ">=", ">", "instanceof").! ~/ bitShift).rep).map(foldBinExpr)
  def eqNoteq[_: P] = P(relational ~ (StringIn("==", "!=").! ~/ relational).rep).map(foldBinExpr)
  def bitAnd[_: P] = P(eqNoteq ~ ("&".! ~ !"&" ~/ eqNoteq).rep).map(foldBinExpr)
  def bitXor[_: P] = P(bitAnd ~ ("^".! ~/ bitAnd).rep).map(foldBinExpr)
  def bitOr[_: P] = P(bitXor ~ ("|".! ~ !"|" ~/ bitXor).rep).map(foldBinExpr)
  def logicAnd[_: P] = P(bitOr ~ ("&&".! ~/ bitOr).rep).map(foldBinExpr)
  def logicOr[_: P] = P(logicAnd ~ ("||".! ~/ logicAnd).rep).map(foldBinExpr)
  //TODO add ternary and assignment
  def expr[_: P]: P[Expr] = P(logicOr)

  val foldBinExpr: ((Expr, Seq[(String, Expr)])) => Expr = { case (left, reps) =>
    reps.foldLeft(left) { case (lhs, (ops, rhs)) =>
      new BinaryExpr(lhs, ops, rhs)
    }
  }
}
