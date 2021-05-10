package verity.ast.infile

import verity.ast._

import scala.collection.mutable.ListBuffer

trait ResolvedOrUnresolvedExpr extends HasText, HasType, Tree

sealed trait Expr extends Tree, HasText, HasType, ResolvedOrUnresolvedExpr {
  // private var _exprType: Type = ToBeInferred(ObjectType, NothingType, List.empty)
  def typ: Type
  // private[verity] def exprType_=(typ: Type) = _exprType = typ
}

sealed trait Literal extends Expr

trait BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  def typ: Type = PrimitiveTypeDef.BOOLEAN
}
case class TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
case class FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
object BoolLiteral {
  def apply(token: Token): BoolLiteral =
    if (token.text.charAt(0) == 't') TrueLiteral(token.textRange)
    else FalseLiteral(token.textRange)
}

sealed trait NumLiteral(val numType: NumType) extends Literal {
  val typ: Type = numType.typ

  def literal: Text
}

enum IntegerLiteral(numType: NumType) extends NumLiteral(numType) {
  case ByteLiteral(literal: Text) extends IntegerLiteral(NumType.BYTE)
  case ShortLiteral(literal: Text) extends IntegerLiteral(NumType.SHORT)
  case IntLiteral(literal: Text) extends IntegerLiteral(NumType.INT)
  case LongLiteral(literal: Text) extends IntegerLiteral(NumType.LONG)
  
  override def text: String = literal.text
  override def textRange: TextRange = literal.textRange
}

enum FloatingLiteral(numType: NumType) extends NumLiteral(numType) {
  case FloatLiteral(literal: Text) extends FloatingLiteral(NumType.FLOAT)
  case DoubleLiteral(literal: Text) extends FloatingLiteral(NumType.DOUBLE)

  override def text: String = literal.text
  override def textRange: TextRange = literal.textRange
}

enum NumType(val typ: Type) {
  case BYTE extends NumType(PrimitiveTypeDef.BYTE)
  case SHORT extends NumType(PrimitiveTypeDef.SHORT)
  case INT extends NumType(PrimitiveTypeDef.INT)
  case LONG extends NumType(PrimitiveTypeDef.LONG)
  case FLOAT extends NumType(PrimitiveTypeDef.FLOAT)
  case DOUBLE extends NumType(PrimitiveTypeDef.DOUBLE)
}

case class StringLiteral(literal: Text) extends Expr {
  val typ: Type = BuiltinTypes.stringType

  override def text: String = literal.text
  override def textRange: TextRange = literal.textRange
}

class ThisRef(val cls: Classlike, override val textRange: TextRange) extends Expr {
  override val typ: Type = cls.makeRef
  override def text: String = "this"
}

class SuperRef(val superCls: Classlike, override val textRange: TextRange) extends Expr {
  override val typ: Type = superCls.makeRef
  override def text: String = "super"
}

/**
 * A resolved reference to a variable
 */
case class VarRef(varName: Text, decl: VariableDecl) extends Expr {
  override def typ: Type = decl.typ
  override def text: String = varName.text
  override def textRange: TextRange = varName.textRange
}

/**
 * TODO figure out a way to avoid the var
 * Used for referring to classes when calling static methods or accessing static
 * fields, NOT used like ResolvedTypeRef or UnresolvedTypeRef
 */
case class ClassRef(cls: Classlike, path: Iterable[Text]) extends Tree, HasText {
  override def text: String = HasText.seqText(path, ".", "", "")
}

/**
 * Used for referring to references to packages such as `foo.bar.baz`
 */
case class PkgRef(path: Seq[Text], pkg: Pkg) extends HasText {
  override def text: String = path.view.map(_.text).mkString(".")
  override def textRange: TextRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

case class ParenExpr(expr: Expr, override val textRange: TextRange) extends Expr {
  def typ: Type = expr.typ
  override def text = s"(${expr.text})"
}

// case class DotChainedExpr(expr: Expr, propertyName: Text) extends Expr {
//   override def text: String = s"${expr.text}.${propertyName}"
//   override lazy val textRange = TextRange(expr.textRange.start, propertyName.textRange.end)
// }

case class ArraySelect(arr: Expr, index: Expr, bracketsTextRange: TextRange) extends Expr {
  override def typ: Type = (arr.typ: @unchecked) match {
    case at: ArrayType => at.elemType
  }
  override def text: String = s"${arr.text}[${index.text}]"
  override def textRange: TextRange = TextRange(arr.textRange.start, bracketsTextRange.end)
}

case class BinaryExpr(left: Expr, op: Op, right: Expr, typ: Type) extends Expr {
  override def text: String = s"(${left.text} ${op.text} ${right.text})"
  override def textRange: TextRange = TextRange(left.textRange.start, right.textRange.end)
}

case class UnaryPreExpr(op: Op, expr: Expr, typ: Type) extends Expr {
  override def text: String = s"(${op.text} ${expr.text})"
  override def textRange: TextRange = TextRange(op.textRange.start, expr.textRange.end)
}

case class UnaryPostExpr(expr: Expr, op: Op, typ: Type) extends Expr {
  override def text: String = s"(${expr.text}${op.text})"
  override def textRange: TextRange = TextRange(expr.textRange.start, op.textRange.end)
}

case class AssignmentExpr(lhs: Expr, rhs: Expr, extraOp: Option[Token]) extends Expr {
  override def typ: Type = lhs.typ
  override def text: String = lhs.text + extraOp.fold("")(_.text) + "=" + rhs.text
  override def textRange: TextRange = TextRange(lhs.textRange.start, rhs.textRange.end)
}

class InstanceOf(val expr: Expr, override val textRange: TextRange) extends Expr {
  override val typ: Type = PrimitiveType.BooleanType
  override def text = s"${expr.text} instanceof ${typ.text}"
}

/** An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(opType: OpType, override val textRange: TextRange) extends Tree, HasText {
  override def text: String = opType.text
}
enum OpType(val text: String) {
  case ADD extends OpType("+")
  case SUBTRACT extends OpType("-")
  case MULTIPLY extends OpType("*")
  case DIVIDE extends OpType("/")
  case MOD extends OpType("%")
  case RSHIFT extends OpType(">>")
  case RSHIFT_UNSIGNED extends OpType(">>>")
  case LSHIFT extends OpType("<<")
  case BIT_AND extends OpType("&")
  case BIT_OR extends OpType("|")
  case BIT_XOR extends OpType("^")
  case LOGIC_AND extends OpType("&&")
  case LOGIC_OR extends OpType("||")
  case GREATER extends OpType(">")
  case LESS extends OpType("<")
  case EQUAL extends OpType("==")
  case GREATER_OR_EQ extends OpType(">=")
  case LESS_OR_EQ extends OpType("<=")
  case NOT_EQ extends OpType("!=")
}
object OpType {
  def findBySymbol(symbol: String): Option[OpType] = OpType.values.find(_.text == symbol)
}

class Block(val stmts: ListBuffer[Statement], override val textRange: TextRange, private[this] var _typ: Type) extends Expr, Statement {
  override def typ: Type = _typ
  private[verity] def typ_=(newTyp: Type): Unit = _typ = newTyp
  override def text: String = stmts.map(_.text).mkString("{", "", "}")
}
object Block {
  def empty(typ: Type): Block = Block(ListBuffer.empty, TextRange.synthetic, typ)
}

case class MethodCall (
    caller: Option[Expr | ClassRef],
    methodName: Text,
    valArgs: ArgList,
    typeArgs: TypeArgList,
    givenArgs: Option[ArgList] = None, //TODO do givenArgs!!
    proofArgs: Option[ArgList] = None,  //TODO do proofArgs!!
    typ: Type,
    resolved: Method
) extends Expr {
  //TODO figure out how to print types
  override def text: String = caller.fold("")(_.text + ".")
    + typeArgs.text
    + methodName.text
    + valArgs.text
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)
  // override def textRange = ???
}

case class ArgList(args: List[Expr], argsKind: ArgsKind, override val textRange: TextRange) extends HasText {
  override def text: String = args.view.map(_.text).mkString("(", ",", ")")
}

enum ArgsKind {
  case Normal
  case Given(textRange: TextRange)
  case Proof(textRange: TextRange)
}

class FieldAccess(obj: Expr, field: Field, fieldNameRange: TextRange) extends Expr, HasText {
  override def typ: Type = field.typ
  override def textRange: TextRange = TextRange(obj.textRange.start, fieldNameRange.end)
  override def text = s"${obj.text}.${field.name}"
}

class StaticFieldAccess(clsRef: ClassRef, field: Field, fieldNameRange: TextRange) extends Expr, HasText {
  override def typ: Type = field.typ
  override def textRange: TextRange = TextRange(clsRef.textRange.start, fieldNameRange.end)
  override def text = s"${clsRef.text}.${field.name}"
}
