package verity.ast

import verity.ast.*

import scala.collection.mutable.ArrayBuffer

trait ResolvedOrUnresolvedExpr extends HasTextRange, HasType, Tree

sealed trait Expr extends Tree, HasText, HasTextRange, HasType, ResolvedOrUnresolvedExpr {
  def typ: Type
}

object Expr {
  def dummy(typ: Type, textRange: TextRange = TextRange.synthetic): Expr = typ match {
    case pt: PrimitiveType => ???
    case _                 => UpcastExpr(NullLiteral(textRange), typ, textRange)
  }
}

sealed trait Literal extends Expr

trait BoolLiteral(override val text: String, override val textRange: TextRange) extends Literal {
  def typ: Type = PrimitiveType.BooleanType
}
case class TrueLiteral(tr: TextRange) extends BoolLiteral("true", tr)
case class FalseLiteral(tr: TextRange) extends BoolLiteral("false", tr)

sealed trait NumLiteral(val typ: PrimitiveType) extends Literal {}

enum IntegerLiteral(typ: PrimitiveType) extends NumLiteral(typ) {
  case ByteLiteral(text: String, override val textRange: TextRange)
      extends IntegerLiteral(PrimitiveType.ByteType)
  case ShortLiteral(text: String, override val textRange: TextRange)
      extends IntegerLiteral(PrimitiveType.ShortType)
  case IntLiteral(text: String, override val textRange: TextRange)
      extends IntegerLiteral(PrimitiveType.IntType)
  case LongLiteral(text: String, override val textRange: TextRange)
      extends IntegerLiteral(PrimitiveType.LongType)
}

enum FloatingLiteral(typ: PrimitiveType) extends NumLiteral(typ) {
  case FloatLiteral(text: String, override val textRange: TextRange)
      extends FloatingLiteral(PrimitiveType.FloatType)
  case DoubleLiteral(text: String, override val textRange: TextRange)
      extends FloatingLiteral(PrimitiveType.DoubleType)
}

class NullLiteral(override val textRange: TextRange) extends Expr {
  override def text = "null"

  override def typ: Type = NothingTypeDef.NothingType
}

case class StringLiteral(text: String, override val textRange: TextRange) extends Expr {
  def typ: Type = BuiltinTypes.stringTypeDef.makeRef
}

class ThisRef(val cls: Classlike, override val textRange: TextRange) extends Expr {
  override val typ: Type = cls.makeRef
  override def text: String = "this"
}

class SuperRef(val superCls: Classlike, override val textRange: TextRange) extends Expr {
  override var typ: Type = superCls.makeRef
  override def text: String = "super"
}

/** A resolved reference to a variable
  */
case class VarRef(varName: Text, decl: VariableDecl) extends Expr {
  override def typ: Type = decl.typ
  override def text: String = varName.text
  override def textRange: TextRange = varName.textRange
}

/** Used for referring to classes when calling static methods or accessing static
  * fields, NOT used like ResolvedTypeRef or UnresolvedTypeRef
  */
case class ClassRef(cls: Classlike, path: Iterable[Text]) extends Tree, HasTextRange {
  override def text: String = HasText.seqText(path, ".", "", "")
  override def textRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

/** Used for referring to references to packages such as `foo.bar.baz`
  */
case class PkgRef(path: Seq[Text], pkg: Pkg) extends HasText {
  override def text: String = path.view.map(_.text).mkString(".")
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
  override def textRange = TextRange(arr.textRange.start, bracketsTextRange.end)
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

case class AssignmentExpr(lhs: Expr, rhs: Expr, extraOp: Option[Text]) extends Expr {
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
case class Op(opType: OpType, override val textRange: TextRange) extends Tree, HasTextRange {
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

class Block(
  val stmts: ArrayBuffer[Statement],
  override val textRange: TextRange,
  private[this] var _typ: Type
) extends Expr,
      Statement {
  override def typ: Type = _typ
  private[verity] def typ_=(newTyp: Type): Unit = _typ = newTyp
  override def text: String = stmts.map(_.text).mkString("{", "", "}")
}
object Block {
  def empty(typ: Type): Block = Block(ArrayBuffer.empty, TextRange.synthetic, typ)
}

case class MethodCall(
  caller: Option[Expr | ClassRef],
  methodName: Text,
  valArgs: ArgList,
  typeArgs: Option[TypeArgList],
  givenArgs: Option[ArgList] = None, //TODO do givenArgs!!
  proofArgs: Option[ArgList] = None, //TODO do proofArgs!!
  typ: Type,
  resolved: Method
) extends Expr {
  //TODO figure out how to print types
  override def text: String = caller.fold("")(_.text + ".")
    + typeArgs.fold("")(_.text)
    + methodName.text
    + valArgs.text /*
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)*/
  override def textRange = TextRange(
    caller.orElse(typeArgs).getOrElse(methodName).textRange.start,
    proofArgs.orElse(givenArgs).getOrElse(valArgs).textRange.end
  )
}

/** A constructor call that has been resolved
  * @param cls The class this is trying to construct
  */
class CtorCall(
  val cls: ClassRef,
  valArgs: ArgList,
  typeArgs: Option[TypeArgList],
  givenArgs: Option[ArgList],
  proofArgs: Option[ArgList],
  typ: Type,
  resolved: Method,
  newKeywordPos: Position
) extends MethodCall(
      Some(cls),
      Text("<init>"),
      valArgs,
      typeArgs,
      givenArgs,
      proofArgs,
      typ,
      resolved
    ) {
  override def text: String = "new " + cls.text
    + typeArgs.fold("")(_.text)
    + valArgs.text
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)

  override def textRange = TextRange(
    newKeywordPos,
    proofArgs.orElse(givenArgs).getOrElse(valArgs).textRange.end
  )
}

case class ArgList(
  args: List[ResolvedOrUnresolvedExpr],
  argsKind: ArgsKind,
  override val textRange: TextRange
) extends HasTextRange {
  override def text: String = args.view.map(_.text).mkString("(", ",", ")")
}

enum ArgsKind {
  case Normal
  case Given
  case Proof
}

class FieldAccess(obj: Expr, field: Field, fieldNameRange: TextRange) extends Expr {
  override def typ: Type = field.typ
  override def textRange: TextRange = TextRange(obj.textRange.start, fieldNameRange.end)
  override def text = s"${obj.text}.${field.name}"
}

class StaticFieldAccess(clsRef: ClassRef, field: Field, fieldNameRange: TextRange)
    extends Expr,
      HasText {
  override def typ: Type = field.typ
  override def textRange: TextRange = TextRange(clsRef.textRange.start, fieldNameRange.end)
  override def text = s"${clsRef.text}.${field.name}"
}

case class UpcastExpr(expr: Expr, typ: Type, textRange: TextRange) extends Expr {
  override def text = s"(${typ.text}) ${expr.text}"
}
