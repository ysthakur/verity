package verity.parser

import verity.ast._
import verity.ast.infile.unresolved._
import verity.ast.infile.{ResolvedOrUnresolvedExpr => RoUExpr, _}
import verity.parser.Core._
import verity.parser.Types._

import fastparse._
import fastparse.JavaWhitespace._

private object Exprs {

  /** **************************BEGIN ATOMS***************************************
    */
  def boolLiteral[_: P]: P[BoolLiteral] = P(Index ~ ("true".! | "false".!) ~ Index).map {
    case (start, text, end) =>
      if (text(0) == 't') new TrueLiteral(TextRange(start, end))
      else new FalseLiteral(TextRange(start, end))
  }
  //TODO merge int and float literals, and allow underscores
  def intLiteral[_: P]: P[IntegerLiteral] =
    P(Index ~ CharsWhileIn("0-9").! ~ CharIn("bBsSlL").?.! ~ Index).map {
      case (start, num, suffix, end) =>
        val range = TextRange(start, end)
        import IntegerLiteral._
        suffix match {
          case "l" | "L" => LongLiteral(num, range)
          case "s" | "S" => ShortLiteral(num, range)
          case "b" | "B" => ByteLiteral(num, range)
          case _         => IntLiteral(num, range)
        }
    }
  def numLiteral[_: P]: P[IntegerLiteral] = P(intLiteral)
  def stringLiteral[_: P]: P[StringLiteral] =
    P("\"" ~/ Index ~ (("\\" ~/ AnyChar) | (!"\"" ~ AnyChar)).rep.! ~ "\"" ~ Index).map {
      case (start, text, end) => StringLiteral("\"" + text + "\"", TextRange(start, end))
    }
  def thisRef[_: P]: P[UnresolvedThisRef] = P(Index ~ "this" ~ Index).map { case (start, end) =>
    new UnresolvedThisRef(TextRange(start, end))
  }
  def superRef[_: P]: P[UnresolvedSuperRef] = P(Index ~ "super" ~ Index).map { case (start, end) =>
    new UnresolvedSuperRef(TextRange(start, end))
  }
  def literal[_: P]: P[RoUExpr] = P(numLiteral | boolLiteral | stringLiteral | thisRef | superRef)
  // def varRef[_: P] = P(Index ~ identifier ~ Index).map { case (start, varName, end) =>
  //   new VarRef(varName, TextRange(start, end))
  // }
  def parenExpr[_: P]: P[UnresolvedParenExpr] = P(Index ~ "(" ~ expr ~/ ")" ~ Index).map {
    case (start, expr, end) =>
      new UnresolvedParenExpr(expr, TextRange(start, end))
  }

  //TODO parse given and proof arguments!!!
  def noObjMethodCall[_: P]: P[UnresolvedMethodCall] =
    P(typeArgList ~ identifierText ~ methodArgList).map { case (typeArgs, name, args) =>
      UnresolvedMethodCall(None, name, args, Some(typeArgs), None, None)
    }

  def varRefOrMethodCall[_: P]: P[RoUExpr] =
    P(
        identifierText ~ ("." ~ identifierText ~ !"(").rep ~ ("." ~/ typeArgList ~ identifierText ~ methodArgList).?
    ).map {
      case (first, rest, Some((typeArgs, name, valArgs))) =>
        UnresolvedMethodCall(
          Some(MultiDotRef(first +: rest)),
          name,
          valArgs,
          Some(typeArgs),
          None,
          None
        )
      case (first, rest, None) => MultiDotRefExpr(first +: rest)
    }

  //todo add constructor call
  def selectable[_: P]: P[RoUExpr] = P(parenExpr | literal | noObjMethodCall | varRefOrMethodCall)

  /** **************************END ATOMS***************************************
    */

  /** **************************BEGIN TOP-LEVEL STUFF AFTER ATOMS***************************************
    */
  def fieldAccessOrMethodCall[_: P]: P[RoUExpr => RoUExpr] =
    P(
        "." ~ identifierText ~/ methodArgList.?
    ).map {
      case (name, Some(args)) =>
        obj =>
          new UnresolvedMethodCall(
            Some(obj),
            name,
            args,
            None,
            None,
            None
          )
      case (name, _) =>
        prev => UnresolvedFieldAccess(prev, name)
    }

  def typeParamsFirstMethodCall[_: P]: P[HasTextRange => RoUExpr] = P(
      "." ~ typeArgList ~ identifierText ~ methodArgList
  ).map { case (typeArgs, name, valArgs /*, givenArgs*/ ) =>
    caller =>
      UnresolvedMethodCall(
        Some(caller),
        name,
        valArgs,
        Some(typeArgs),
        None,
        None
      )
  }
  def arrayAccess[_: P]: P[RoUExpr => RoUExpr] = P("[" ~/ Index ~ expr ~ "]" ~ Index).map {
    case (start, index, end) => arr => UnresolvedArraySelect(arr, index, TextRange(start, end))
  }

  def topExpr[_: P]: P[RoUExpr] =
    P(selectable ~ (fieldAccessOrMethodCall | typeParamsFirstMethodCall | arrayAccess).rep).map {
      case (expr, ctors) => ctors.foldLeft(expr)((e, f) => f(e))
    }

  /** **************************END TOP-LEVEL STUFF AFTER ATOMS***************************************
    */

  /** **************************BEGIN OPERATORS***************************************
    */
  def mulDivMod[_: P]: P[RoUExpr] =
    P(topExpr ~ (Index ~ CharIn("*/%").! ~/ Index ~ topExpr).rep).map(foldBinExpr)
  //noinspection MutatorLikeMethodIsParameterless
  // def mulDivMod[_: P] = binExpr(CharIn("*/%").!, topExpr)
  def addSub[_: P]: P[RoUExpr] =
    P(mulDivMod ~ (Index ~ CharIn("+\\-").! ~/ Index ~ mulDivMod).rep).map(foldBinExpr)
  def bitShift[_: P]: P[RoUExpr] =
    P(addSub ~ (Index ~ StringIn(">>>", ">>", "<<").! ~/ Index ~ addSub).rep).map(foldBinExpr)
  def relational[_: P]: P[RoUExpr] =
    P(
        bitShift ~
          ((StringIn("<=", "<", ">=", ">").! ~/ Index ~ bitShift)
            | ("instanceof".! ~ Index ~/ nonWildcardType)).rep
    ).map { case (first, reps) =>
      reps.foldLeft(first) { case (lhs, (op, opEnd, rhs)) =>
        val opRange = TextRange(opEnd - op.length, opEnd)
        op match {
          case "instanceof" =>
            new UnresolvedInstanceOf(lhs, rhs.asInstanceOf[UnresolvedTypeRef], opRange)
          case _ =>
            UnresolvedBinaryExpr(
                lhs,
                Op(OpType.findBySymbol(op).get, opRange),
                rhs.asInstanceOf[RoUExpr]
            )
        }
      }
    }
  def eqNoteq[_: P]: P[RoUExpr] =
    P(relational ~ (Index ~ StringIn("==", "!=").! ~/ Index ~ relational).rep).map(foldBinExpr)
  def bitAnd[_: P]: P[RoUExpr] =
    P(eqNoteq ~ (Index ~ "&".! ~ !"&" ~/ Index ~ eqNoteq).rep).map(foldBinExpr)
  def bitXor[_: P]: P[RoUExpr] = P(bitAnd ~ (Index ~ "^".! ~/ Index ~ bitAnd).rep).map(foldBinExpr)
  def bitOr[_: P]: P[RoUExpr] =
    P(bitXor ~ (Index ~ "|".! ~ !"|" ~/ Index ~ bitXor).rep).map(foldBinExpr)
  def logicAnd[_: P]: P[RoUExpr] = P(bitOr ~ (Index ~ "&&".! ~/ Index ~ bitOr).rep).map(foldBinExpr)
  def logicOr[_: P]: P[RoUExpr] =
    P(logicAnd ~ (Index ~ "||".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  //TODO add ternary and assignment
  def expr[_: P]: P[RoUExpr] = P(logicOr)

  /** **************************END OPERATORS***************************************
    */

  //todo allow accessing fields and modifying arrays
  // def assignExpr[_: P] = P(identifierText ~ (Index ~ "=".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  val foldBinExpr: ((RoUExpr, Seq[(Int, String, Int, RoUExpr)])) => RoUExpr = { case (left, reps) =>
    reps.foldLeft(left) { case (lhs, (opStart, op, opEnd, rhs)) =>
      UnresolvedBinaryExpr(lhs, Op(OpType.findBySymbol(op).get, TextRange(opStart, opEnd)), rhs)
    }
  }

  def methodArgList[_: P]: P[UnresolvedArgList] =
    P("(" ~/ Index ~ (expr ~ ("," ~ expr).rep).? ~ ")" ~ Index).map {
      case (start, None, end) => UnresolvedArgList(Nil, ArgsKind.Normal, TextRange(start, end))
      case (start, Some((firstArg, args)), end) =>
        UnresolvedArgList(firstArg :: args.toList, ArgsKind.Normal, TextRange(start, end))
    }
}
