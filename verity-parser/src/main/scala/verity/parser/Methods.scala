package verity.parser

import verity.ast._, infile._
import Core._
import Exprs._

import fastparse._, JavaWhitespace._

import collection.mutable.ListBuffer

private object Methods {
  def param[_: P] = P(typeRef ~ Index ~ identifier ~ Index).map {
    case (typ, nameStart, name, nameEnd) =>
      Parameter(
          List.empty, //TODO Add annotations!
          typ,
          name,
          false,
          false,
          TextRange(typ.textRange.start, nameEnd)
      )
  }
  //TODO make separate givenParamList pattern
  def paramList[_: P] = P(Index ~ "(" ~/ param.rep ~ ")" ~ Index).map { case (start, params, end) =>
    ParamList(params.toList, TextRange(start, end))
  }

  def exprStmt[_: P] = P(expr ~ ";" ~ Index).map { case (expr, end) =>
    new ExprStmt(expr, end)
  }

  //todo allow final modifier
  def localVars[_: P] = P(modifiers ~ typeRef ~ Index ~ identifier ~ Index ~ ("=" ~ expr).? ~ ";" ~ Index).map {
    case (mods, typ, idStart, name, idEnd, expr, end) =>
      new LocalVar(mods, Text(name, TextRange(idStart, idEnd)), typ, expr, false, end)
  }

  def returnStmt[_: P] = P("return" ~ Index ~/ expr ~ ";" ~ Index).map { case (start, expr, end) =>
    new ReturnStmt(expr, TextRange(start - 6, end))
  }

  //TODO add control flow, etc. (BEFORE exprStmt)
  def stmt[_: P]: P[Statement] = P(exprStmt | returnStmt | localVars)

  def block[_: P]: P[Block] = P(Index ~ "{" ~ (stmt ~ ";".rep).rep ~ "}" ~ Index).map {
    case (start, stmts, end) => Block(stmts.to(ListBuffer), TextRange(start, end))
  }

  //TODO add type parameters
  def normMethod[_: P]: P[Method] =
    P(modifiers ~ typeParamList.? ~ typeRef ~ identifierText ~ paramList ~ (block | ";" ~ Index)).map {
      case (modifiers, typeParams, returnType, name, params, body) =>
        new NormMethod(
            modifiers.to(ListBuffer),
            typeParams.getOrElse(TypeParamList.empty),
            returnType,
            name,
            params,
            None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
            None,
            body match {
              case b: Block => Some(b)
              case _        => None
            },
            TextRange(
                (if (modifiers.isEmpty) returnType else modifiers.head).textRange.start,
                (body: @unchecked) match {
                  case b: Block => b.textRange.end
                  case e: Int   => e
                }
            )
        )
    }

  /** A constructor
    */
  def ctor[_: P]: P[(() => HasCtors) => Constructor] =
    P(Index ~ modifiers ~ identifierWithTextRange ~ paramList ~ block).map {
      case (start, modifiers, (name, nameRange), params, body) =>
        cls => new Constructor(
            modifiers.to(ListBuffer),
            Text(name, nameRange),
            params,
            None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
            None,
            body,
            TextRange(start, body.textRange.end),
            cls
        )
    }

  def ctor2[_: P]: P[Seq[Modifier] => (() => HasCtors) => Any] =
    P(identifierText ~ &("(") ~/ paramList ~ block).map {
      case (name, params, body) =>
        modifiers => cls => new Constructor(
            modifiers.to(ListBuffer),
            name,
            params,
            None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
            None,
            body,
            TextRange((if (modifiers.isEmpty) name else modifiers.head).textRange.start, body.textRange.end),
            cls
        )
    }

  def methodWithTypeParams[_: P] =
    P(typeParamList ~/ typeRef ~ identifierText ~ paramList ~ (block | ";" ~ Index)).map {
      case (typeParams, returnType, name, params, body) =>
        (modifiers: Seq[Modifier]) =>
          new NormMethod(
              modifiers.to(ListBuffer),
              typeParams,
              returnType,
              name,
              params,
              None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
              None,
              body match {
                case b: Block => Some(b)
                case _        => None
              },
              TextRange(
                  (if (modifiers.isEmpty) returnType else modifiers.head).textRange.start,
                  (body: @unchecked) match {
                    case b: Block => b.textRange.end
                    case e: Int   => e
                  }
              )
          )
    }

  def methodWithoutTypeParams[_: P]: P[(Type, Text) => Seq[Modifier] => Any] =
    P(paramList ~ (block | ";" ~ Index)).map {
      case (params, body) =>
        (returnType: Type, name: Text) => (modifiers: Seq[Modifier]) =>
          new NormMethod(
              modifiers.to(ListBuffer),
              TypeParamList.empty,
              returnType,
              name,
              params,
              None, //TODO PARSE GIVEN AND PROOF PARAMETERS!!!
              None,
              body match {
                case b: Block => Some(b)
                case _        => None
              },
              TextRange(
                  (if (modifiers.isEmpty) returnType else modifiers.head).textRange.start,
                  (body: @unchecked) match {
                    case b: Block => b.textRange.end
                    case e: Int   => e
                  }
              )
          )
    }
}
