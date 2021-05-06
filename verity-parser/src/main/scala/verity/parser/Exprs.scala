package verity.parser

import verity.ast._, infile._
import Core._

import fastparse._, JavaWhitespace._

private object Exprs {
  def boolLiteral[_: P] = P(Index ~ ("true".! | "false".!) ~ Index).map { case (start, text, end) =>
    if (text(0) == 't') new TrueLiteral(TextRange(start, end))
    else new FalseLiteral(TextRange(start, end))
  }
  //TODO merge int and float literals, and allow underscores
  def intLiteral[_: P] = P(Index ~ CharsWhileIn("0-9").! ~ CharIn("bBsSlL").?.! ~ Index).map {
    case (start, num, suffix, end) =>
      val literal = Text(num, TextRange(start, end))
      import IntegerLiteral._
      suffix match {
        case "l" | "L" => LongLiteral(literal)
        case "s" | "S" => ShortLiteral(literal)
        case "b" | "B" => ByteLiteral(literal)
        case _         => IntLiteral(literal)
      }
  }
  def numLiteral[_: P] = P(intLiteral)
  def stringLiteral[_: P] =
    P("\"" ~/ Index ~ (("\\" ~/ AnyChar) | (!"\"" ~ AnyChar)).rep.! ~ "\"" ~ Index).map {
      case (start, text, end) => new StringLiteral(Text("\"" + text + "\"", TextRange(start, end)))
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

  //TODO parse given and proof arguments!!!
  def varRefOrMethodCall[_: P]: P[Expr] =
    P(identifierText ~ ("(" ~/ Index ~ methodArgList ~ ")" ~ Index).?).map {
      case (name, Some((argStart, args, argsEnd))) =>
        new MethodCall(None, name, args, None)
      case (name, _) => new DotlessRef(name)
    }

  def dotRefOrMethodCall[_: P]: P[Expr => Expr] =
    P(
        "." ~ identifierText ~/ ("(" ~/ Index ~ methodArgList ~ ")" ~ Index).?
    ).map {
      case (name, Some((argStart, args, argsEnd))) =>
        obj => new MethodCall(Some(obj), name, args, None)
      case (name, _) =>
        prev => new DotRef(prev, name)
    }
  def dotChainedRef[_: P]: P[Seq[Text]] =
    P(identifierText ~ !"(" ~ ("." ~ identifierText ~ !"(").rep).map { case (head, rest) =>
      head +: rest
    }

  def methodArgList[_: P] =
    P("(" ~/ Index ~ (expr ~ ("," ~ expr).rep).? ~ ")" ~ Index).map {
      case (start, None, end) => ArgList(Nil, ArgsKind.Normal, TextRange(start, end))
      case (start, Some((firstArg, args)), end) =>
        ArgList(firstArg :: args.toList, ArgsKind.Normal, TextRange(start, end))
    }

  def selectable[_: P]: P[Expr] = P(parenExpr | literal | varRefOrMethodCall)

  def typeParamsFirstMethodCall[_: P]: P[Expr => Expr] = P(
      "." ~ "<" ~/ Index ~ typeArgList ~ ">" ~ Index ~ identifierText ~ methodArgList
  ).map { case (typeArgStart, typeArgs, typeArgEnd, name, valArgs /*, givenArgs*/ ) =>
    (obj: Expr) =>
      new MethodCall(
          Some(obj),
          name,
          valArgs,
          Some(TypeArgList(typeArgs, TextRange(typeArgStart, typeArgEnd)))
          /*, givenArgs */
      )
  }
  def arrayAccess[_: P]: P[Expr => Expr] = P("[" ~/ Index ~ expr ~ "]" ~ Index).map {
    case (start, index, end) => arr => ArraySelect(arr, index, TextRange(start, end))
  }

  def topExpr[_: P]: P[Expr] =
    P(selectable ~ (dotRefOrMethodCall | typeParamsFirstMethodCall | arrayAccess).rep).map {
      case (expr, ctors) => ctors.foldLeft(expr)((e, f) => f(e))
    }

  def mulDivMod[_: P] =
    P(topExpr ~ (Index ~ CharIn("*/").! ~/ Index ~ topExpr).rep).map(foldBinExpr)
  // def mulDivMod[_: P] = binExpr(CharIn("*/%").!, topExpr)
  def addSub[_: P] =
    P(mulDivMod ~ (Index ~ CharIn("+\\-").! ~/ Index ~ mulDivMod).rep).map(foldBinExpr)
  def bitShift[_: P] =
    P(addSub ~ (Index ~ StringIn(">>>", ">>", "<<").! ~/ Index ~ addSub).rep).map(foldBinExpr)
  def relational[_: P] =
    P(
        bitShift ~
          (Index ~ StringIn("<=", "<", ">=", ">").! ~/ Index ~ bitShift
            | Index ~ "instanceof".! ~ Index ~/ typeRef).rep
    ).map { case (first, reps) =>
      reps.foldLeft(first) { case (lhs, (opStart, op, opEnd, rhs)) =>
        val opRange = TextRange(opStart, opEnd)
        op match {
          case "instanceof" => new InstanceOf(lhs, rhs.asInstanceOf[TypeRef], opRange)
          case _ =>
            new BinaryExpr(lhs, Op(OpType.findBySymbol(op).get, opRange), rhs.asInstanceOf[Expr])
        }
      }
    }
  def eqNoteq[_: P] =
    P(relational ~ (Index ~ StringIn("==", "!=").! ~/ Index ~ relational).rep).map(foldBinExpr)
  def bitAnd[_: P] = P(eqNoteq ~ (Index ~ "&".! ~ !"&" ~/ Index ~ eqNoteq).rep).map(foldBinExpr)
  def bitXor[_: P] = P(bitAnd ~ (Index ~ "^".! ~/ Index ~ bitAnd).rep).map(foldBinExpr)
  def bitOr[_: P] = P(bitXor ~ (Index ~ "|".! ~ !"|" ~/ Index ~ bitXor).rep).map(foldBinExpr)
  def logicAnd[_: P] = P(bitOr ~ (Index ~ "&&".! ~/ Index ~ bitOr).rep).map(foldBinExpr)
  def logicOr[_: P] = P(logicAnd ~ (Index ~ "||".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  //TODO add ternary and assignment
  def expr[_: P]: P[Expr] = P(logicOr)

  //todo allow accessing fields and modifying arrays
  // def assignExpr[_: P] = P(identifierText ~ (Index ~ "=".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  val foldBinExpr: ((Expr, Seq[(Int, String, Int, Expr)])) => Expr = { case (left, reps) =>
    reps.foldLeft(left) { case (lhs, (opStart, op, opEnd, rhs)) =>
      new BinaryExpr(lhs, Op(OpType.findBySymbol(op).get, TextRange(opStart, opEnd)), rhs)
    }
  }
}
