package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

sealed trait Expr extends Tree, HasText {
  // private var _exprType: Type = ToBeInferred(ObjectType, NothingType, List.empty)
  def exprType: Type
  // private[verity] def exprType_=(typ: Type) = _exprType = typ
}

trait UnresolvedType extends Expr {
  private[this] var _exprType: Type = ToBeInferred(BuiltinTypes.objectType, NothingType, List.empty)
  override def exprType: Type = _exprType
  private[verity] def exprType_=(typ: Type) = _exprType = typ
}

sealed trait Literal extends Expr

trait BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  def exprType = PrimitiveType.BOOLEAN
}
case class TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
case class FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)
object BoolLiteral {
  def apply(token: Token) =
    if (token.text.charAt(0) == 't') TrueLiteral(token.textRange)
    else FalseLiteral(token.textRange)
}

sealed trait NumLiteral(val numType: NumType) extends Literal {
  def literal: Text

  val exprType = numType.typ
}

enum IntegerLiteral(numType: NumType) extends NumLiteral(numType) {
  case ByteLiteral(literal: Text) extends IntegerLiteral(NumType.BYTE)
  case ShortLiteral(literal: Text) extends IntegerLiteral(NumType.SHORT)
  case IntLiteral(literal: Text) extends IntegerLiteral(NumType.INT)
  case LongLiteral(literal: Text) extends IntegerLiteral(NumType.LONG)
  
  def text = literal.text
  def textRange = literal.textRange
}

enum FloatingLiteral(numType: NumType) extends NumLiteral(numType) {
  case FloatLiteral(literal: Text) extends FloatingLiteral(NumType.FLOAT)
  case DoubleLiteral(literal: Text) extends FloatingLiteral(NumType.DOUBLE)

  def text = literal.text
  def textRange = literal.textRange
}

enum NumType(val typ: Type) {
  case BYTE extends NumType(PrimitiveType.BYTE)
  case SHORT extends NumType(PrimitiveType.SHORT)
  case INT extends NumType(PrimitiveType.INT)
  case LONG extends NumType(PrimitiveType.LONG)
  case FLOAT extends NumType(PrimitiveType.FLOAT)
  case DOUBLE extends NumType(PrimitiveType.DOUBLE)
}

case class StringLiteral(val literal: Text) extends Expr {
  val exprType = BuiltinTypes.stringType

  def text = literal.text
  def textRange = literal.textRange
}

class ThisRef(override val textRange: TextRange) extends Expr, UnresolvedType {
  override def text: String = "this"
}

class SuperRef(override val textRange: TextRange) extends Expr, UnresolvedType {
  override def text: String = "super"
}

/**
 * An unresolved reference to a variable, package, or class
 */
case class DotlessRef(refName: Text) extends Expr {
  override def text = refName.text
  override def textRange = refName.textRange

  def exprType = ???
}

/**
 * A resolved reference to a variable
 */
case class VarRef(varName: Text, decl: VariableDecl) extends Expr, UnresolvedType {
  override def text = varName.text
  override def textRange = varName.textRange
}

case class DotRef(val first: Expr, val selected: Text)
    extends Tree,
      Expr,
      HasText, UnresolvedType {
  override def textRange = TextRange(first.textRange.start, selected.textRange.end)
  override def text = s"${first.text}.$selected"
}

case class MultiDotRef(val path: Seq[Text]) {
  def text = path.view.map(_.text).mkString(".")
  def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}


/**
 * Used for referring to classes when calling static methods or accessing static
 * fields, NOT used like TypeRef
 */
case class ClassRef(val clsName: Text, cls: Classlike, pkgRef: Option[PkgRef]) extends HasText {
  def text = clsName.text
  def textRange = clsName.textRange
}

/**
 * Used for referring to references to packages such as `foo.bar.baz`
 */
case class PkgRef(val path: Seq[Text], pkg: Package) extends HasText {
  def text = path.view.map(_.text).mkString(".")
  def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

case class ParenExpr(expr: Expr, override val textRange: TextRange) extends Expr {
  def exprType = expr.exprType
  def text = s"(${expr.text})"
}

// case class DotChainedExpr(expr: Expr, propertyName: Text) extends Expr {
//   override def text: String = s"${expr.text}.${propertyName}"
//   override lazy val textRange = TextRange(expr.textRange.start, propertyName.textRange.end)
// }

case class ArraySelect(arr: Expr, index: Expr, val bracketsTextRange: TextRange) extends Expr, UnresolvedType {
  override def text: String = s"${arr.text}[${index.text}]"
  override def textRange = TextRange(arr.textRange.start, bracketsTextRange.end)
}

case class BinaryExpr(left: Expr, op: Op, right: Expr) extends Expr, UnresolvedType {
  override def text: String = s"(${left.text} $op ${right.text})"
  override def textRange = TextRange(left.textRange.start, right.textRange.end)
}

case class UnaryPreExpr(op: Op, expr: Expr) extends Expr, UnresolvedType {
  override def text: String = s"(${op.text} ${expr.text})"
  override def textRange = TextRange(op.textRange.start, expr.textRange.end)
}

case class UnaryPostExpr(expr: Expr, op: Op) extends Expr, UnresolvedType {
  override def text: String = s"(${expr.text}${op.text})"
  override def textRange = TextRange(expr.textRange.start, op.textRange.end)
}

case class AssignmentExpr(lhs: Expr, rhs: Expr, extraOp: Option[Token]) extends Expr, UnresolvedType {
  def text = lhs.text + extraOp.fold("")(_.text) + "=" + rhs.text
  override def textRange = TextRange(lhs.textRange.start, rhs.textRange.end)
}

class InstanceOf(val expr: Expr, val exprType: Type, val textRange: TextRange) extends Expr {
  def text = s"${expr.text} instanceof ${exprType.text}"
}

/** An operator
  * @param symbol
  * @param startOffset
  * @param endOffset
  */
case class Op(opType: OpType, textRange: TextRange) extends Tree, HasText {
  def isBinary: Boolean = ???
  override def text: String = opType.text
}
enum OpType(val text: String) {
  case ADD extends OpType("+")
  case SUBTRACT extends OpType("-")
  case MULTIPLY extends OpType("*")
  case DIVIDE extends OpType("/")
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

case class Block(stmts: ListBuffer[Statement], textRange: TextRange) extends Expr, Statement, UnresolvedType {
  def text = stmts.map(_.text).mkString("{", "", "}")
}

case class ToBeGiven(typ: Type) extends Expr, UnresolvedType {
  override def text = ???
  override def textRange = ???
}

case class MethodCall(
    obj: Option[Expr],
    methodName: Text,
    valArgs: ArgList,
    typeArgs: Option[TypeArgList],
    givenArgs: Option[ArgList] = None, //TODO do givenArgs!!
    proofArgs: Option[ArgList] = None  //TODO do proofArgs!!
) extends Expr, UnresolvedType {
  private[verity] var resolved: Option[Method] = None

  //TODO figure out how to print types
  def text: String = obj.fold("")(_.text + ".")
    + HasText.optText(typeArgs)
    + methodName.text
    + valArgs.text
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)
  override def textRange: TextRange = ???
}

case class ArgList(args: List[Expr], argsKind: ArgsKind, textRange: TextRange) extends HasText {
  def text = args.view.map(_.text).mkString("(", ",", ")")
}

enum ArgsKind {
  case Normal
  case Given(textRange: TextRange)
  case Proof(textRange: TextRange)
}

class FieldAccess(obj: Expr, field: Field, fieldNameRange: TextRange) extends Expr, HasText, UnresolvedType {
  override def textRange = TextRange(obj.textRange.start, fieldNameRange.end)
  def text = s"${obj.text}.${field.name}"
}

class StaticFieldAccess(clsRef: ClassRef, field: Field, fieldNameRange: TextRange) extends Expr, HasText, UnresolvedType {
  override def textRange = TextRange(clsRef.textRange.start, fieldNameRange.end)
  def text = s"${clsRef.text}.${field.name}"
}
