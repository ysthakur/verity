package verity.parsing.parser

import Core._

private object Exprs {
  def boolLiteral[_ : P] = P(Index ~ ("true".! | "false".!) ~ Index).map {
    case (start, text, end) =>
      if (text(0) == 't') new BoolLiteral.TrueLiteral(TextRange(start, end))
      else new BoolLiteral.FalseLiteral(TextRange(start, end))
  }
  //TODO merge int and float literals, and allow underscores
  def intLiteral[_ : P] = P(Index ~ CharsWhileIn("0-9").! ~ CharIn("lL").?.! ~ Index).map {
    case (start, num, l, end) => new IntegerLiteral(num, TextRange(start, end), l == "L")
  }
  def numLiteral[_ : P] = P(intLiteral)
  def thisRef[_ : P] = P(Index ~ "this" ~ Index).map { case (start, end) => ThisRef(TextRange(start, end)) }
  def superRef[_ : P] = P(Index ~ "super" ~ Index).map { case (start, end) => SuperRef(TextRange(start, end)) }
  def literal[_ : P] = P(numLiteral | boolLiteral | thisRef | superRef)
  def varRef[_ : P] = P(identifier).map(VarRef.apply)
  def parenExpr[_ : P] = P(Index ~ "(" ~ expr ~/ ")" ~ Index).map {
    case (start, expr, end) => ParenExpr(expr, TextRange(start, end))
  }

  def methodArgList[_ : P] = argList(expr: P[Expr])

  def methodNameAndArgs[_ : P] = P("<" ~/ typeArgList ~ ">" ~ identifier ~ "(" ~ methodArgList ~ ")" ~ "(" ~ "given" ~ methodArgList ~ ")" ~ Index).map {
    case (typeArgs, name, valArgs, givenArgs, end) => (obj: Option[Expr], start: Int) => MethodCall(obj, name, valArgs, givenArgs, typeArgs, TextRange(start, end))
  }
  
  def noDotMethodCall[_ : P]: P[MethodCall] = P(Index ~ methodNameAndArgs).map {
    case (start, methodCallCtor) => methodCallCtor(None, start)
  }

  def selectable[_ : P]: P[Expr] = P(parenExpr | literal | noDotMethodCall | varRef)

  def dotMethodCall[_ : P]: P[Expr] = P(selectable ~ ("." ~ methodNameAndArgs).rep).map {
    case (obj, methodCalls) => methodCalls.foldLeft(obj: Expr) { (obj, methodCallCtor) =>
      methodCallCtor(Some(obj), obj.textRange.start)
    }
  }

  def methodCall[_ : P] = P(dotMethodCall)

  def topExpr[_ : P]: P[Expr] = P(dotMethodCall)

  def mulDivMod[_ : P] = P(topExpr ~ (CharIn("*/").! ~/ topExpr).rep).map(foldBinExpr)
  // def mulDivMod[_: P] = binExpr(CharIn("*/%").!, topExpr)
  def addSub[_: P] = P(mulDivMod ~ (CharIn("*+\\-").! ~/ mulDivMod).rep).map(foldBinExpr)
  def bitShift[_: P] = P(addSub ~ (StringIn(">>>", ">>", "<<").! ~/ addSub).rep).map(foldBinExpr)
  def relational[_: P] = P(bitShift ~ (StringIn("<=", "<", ">=", ">", "instanceof").! ~/ bitShift).rep).map(foldBinExpr)
  def eqNoteq[_: P] = P(relational ~ (StringIn("==", "!=").! ~/ relational).rep).map(foldBinExpr)
  def bitAnd[_: P] = P(eqNoteq ~ ("&".! ~ !"&" ~/ eqNoteq).rep).map(foldBinExpr)
  def bitXor[_: P] = P(bitAnd ~ ("^".! ~/ bitAnd).rep).map(foldBinExpr)
  def bitOr[_: P] = P(bitXor ~ ("|".! ~ !"|" ~/ bitXor).rep).map(foldBinExpr)
  def logicAnd[_: P] = P(bitOr ~ ("&&".! ~/ bitOr).rep).map(foldBinExpr)
  def logicOr[_: P] = P(logicAnd ~ ("||".! ~/ logicAnd).rep).map(foldBinExpr)
  //TODO add ternary and assignment
  def expr[_ : P]: P[Expr] = P(logicOr)

  val foldBinExpr: ((Expr, Seq[(String, Expr)])) => Expr = {
      case (left, reps) => reps.foldLeft(left) {
        case (lhs, (ops, rhs)) => new BinaryExpr(lhs, ops, rhs)
    }
  }
}