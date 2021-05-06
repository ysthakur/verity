package verity.parser

import fastparse.JavaWhitespace._
import fastparse._
import verity.ast._
import verity.ast.infile.Unresolved._
import verity.ast.infile._
import verity.parser.Core._
import verity.parser.Types._

private object Exprs {
  /****************************BEGIN ATOMS****************************************/
  def boolLiteral[_: P]: P[BoolLiteral] = P(Index ~ ("true".! | "false".!) ~ Index).map { case (start, text, end) =>
    if (text(0) == 't') new TrueLiteral(TextRange(start, end))
    else new FalseLiteral(TextRange(start, end))
  }
  //TODO merge int and float literals, and allow underscores
  def intLiteral[_: P]: P[IntegerLiteral] = P(Index ~ CharsWhileIn("0-9").! ~ CharIn("bBsSlL").?.! ~ Index).map {
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
  def numLiteral[_: P]: P[IntegerLiteral] = P(intLiteral)
  def stringLiteral[_: P]: P[StringLiteral] =
    P("\"" ~/ Index ~ (("\\" ~/ AnyChar) | (!"\"" ~ AnyChar)).rep.! ~ "\"" ~ Index).map {
      case (start, text, end) => new StringLiteral(Text("\"" + text + "\"", TextRange(start, end)))
    }
  def thisRef[_: P]: P[ThisRef] = P(Index ~ "this" ~ Index).map { case (start, end) =>
    new ThisRef(TextRange(start, end))
  }
  def superRef[_: P]: P[SuperRef] = P(Index ~ "super" ~ Index).map { case (start, end) =>
    new SuperRef(TextRange(start, end))
  }
  def literal[_: P]: P[Expr] = P(numLiteral | boolLiteral | stringLiteral | thisRef | superRef)
  // def varRef[_: P] = P(Index ~ identifier ~ Index).map { case (start, varName, end) =>
  //   new VarRef(varName, TextRange(start, end))
  // }
  def parenExpr[_: P]: P[ParenExpr] = P(Index ~ "(" ~ expr ~/ ")" ~ Index).map { case (start, expr, end) =>
    new ParenExpr(expr, TextRange(start, end))
  }

  //TODO parse given and proof arguments!!!
  def noObjMethodCall[_: P]: P[Expr] =
    P(typeArgList ~ identifierText ~ "(" ~/ Index ~ methodArgList ~ ")" ~ Index).map {
      case (typeArgs, name, argStart, args, argsEnd) =>
        new UnresolvedMethodCall(None, name, args, typeArgs, None, None)
    }

  def varRefOrMethodCall[_: P]: P[Expr] =
    P(identifierText ~ ("." ~ identifierText ~ !"(").rep ~ ("." ~/ typeArgList ~ identifierText ~ methodArgList).?).map {
      case (first, rest, Some((typeArgs, name, valArgs))) =>
        new UnresolvedMethodCall(
          Some(MultiDotRef(first +: rest)),
          name,
          valArgs,
          typeArgs,
          None,
          None
        )
      case (first, rest, None) =>
        if (rest.isEmpty) new UnresolvedVarRef(first)
        else new MultiDotRefExpr(first +: rest)
    }

  //todo add constructor call
  def selectable[_: P]: P[Expr] = P(parenExpr | literal | noObjMethodCall | varRefOrMethodCall)
  /****************************END ATOMS****************************************/

  /****************************BEGIN TOP-LEVEL STUFF AFTER ATOMS****************************************/
  def fieldAccessOrMethodCall[_: P]: P[Expr => Expr] =
    P(
        "." ~ identifierText ~/ ("(" ~/ Index ~ methodArgList ~ ")" ~ Index).?
    ).map {
      case (name, Some((argStart, args, argsEnd))) =>
        obj => new UnresolvedMethodCall(Some(obj), name, args, TypeArgList(Nil, TextRange.synthetic), None, None)
      case (name, _) =>
        prev => new UnresolvedFieldAccess(prev, name)
    }

  def typeParamsFirstMethodCall[_: P]: P[HasText => Expr] = P(
      "." ~ typeArgList ~ identifierText ~ methodArgList
  ).map { case (typeArgs, name, valArgs /*, givenArgs*/ ) =>
    (caller: HasText) =>
      new UnresolvedMethodCall(
          Some(caller),
          name,
          valArgs,
          typeArgs,
          None,
          None
      )
  }
  def arrayAccess[_: P]: P[Expr => Expr] = P("[" ~/ Index ~ expr ~ "]" ~ Index).map {
    case (start, index, end) => arr => ArraySelect(arr, index, TextRange(start, end))
  }

  def topExpr[_: P]: P[Expr] =
    P(selectable ~ (fieldAccessOrMethodCall | typeParamsFirstMethodCall  | arrayAccess).rep).map {
      case (expr, ctors) => ctors.foldLeft(expr)((e, f) => f(e))
    }
  /****************************END TOP-LEVEL STUFF AFTER ATOMS****************************************/

  /****************************BEGIN OPERATORS****************************************/
  def mulDivMod[_: P]: P[Expr] =
    P(topExpr ~ (Index ~ CharIn("*/").! ~/ Index ~ topExpr).rep).map(foldBinExpr)
  //noinspection MutatorLikeMethodIsParameterless
  // def mulDivMod[_: P] = binExpr(CharIn("*/%").!, topExpr)
  def addSub[_: P]: P[Expr] =
    P(mulDivMod ~ (Index ~ CharIn("+\\-").! ~/ Index ~ mulDivMod).rep).map(foldBinExpr)
  def bitShift[_: P]: P[Expr] =
    P(addSub ~ (Index ~ StringIn(">>>", ">>", "<<").! ~/ Index ~ addSub).rep).map(foldBinExpr)
  def relational[_: P]: P[Expr] =
    P(
        bitShift ~
          (Index ~ StringIn("<=", "<", ">=", ">").! ~/ Index ~ bitShift
            | Index ~ "instanceof".! ~ Index ~/ nonWildcardType).rep
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
  def eqNoteq[_: P]: P[Expr] =
    P(relational ~ (Index ~ StringIn("==", "!=").! ~/ Index ~ relational).rep).map(foldBinExpr)
  def bitAnd[_: P]: P[Expr] = P(eqNoteq ~ (Index ~ "&".! ~ !"&" ~/ Index ~ eqNoteq).rep).map(foldBinExpr)
  def bitXor[_: P]: P[Expr] = P(bitAnd ~ (Index ~ "^".! ~/ Index ~ bitAnd).rep).map(foldBinExpr)
  def bitOr[_: P]: P[Expr] = P(bitXor ~ (Index ~ "|".! ~ !"|" ~/ Index ~ bitXor).rep).map(foldBinExpr)
  def logicAnd[_: P]: P[Expr] = P(bitOr ~ (Index ~ "&&".! ~/ Index ~ bitOr).rep).map(foldBinExpr)
  def logicOr[_: P]: P[Expr] = P(logicAnd ~ (Index ~ "||".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  //TODO add ternary and assignment
  def expr[_: P]: P[Expr] = P(logicOr)
  /****************************END OPERATORS****************************************/

  //todo allow accessing fields and modifying arrays
  // def assignExpr[_: P] = P(identifierText ~ (Index ~ "=".! ~/ Index ~ logicAnd).rep).map(foldBinExpr)

  val foldBinExpr: ((Expr, Seq[(Int, String, Int, Expr)])) => Expr = { case (left, reps) =>
    reps.foldLeft(left) { case (lhs, (opStart, op, opEnd, rhs)) =>
      new BinaryExpr(lhs, Op(OpType.findBySymbol(op).get, TextRange(opStart, opEnd)), rhs)
    }
  }

  def methodArgList[_: P]: P[ArgList] =
    P("(" ~/ Index ~ (expr ~ ("," ~ expr).rep).? ~ ")" ~ Index).map {
      case (start, None, end) => ArgList(Nil, ArgsKind.Normal, TextRange(start, end))
      case (start, Some((firstArg, args)), end) =>
        ArgList(firstArg :: args.toList, ArgsKind.Normal, TextRange(start, end))
    }
}
