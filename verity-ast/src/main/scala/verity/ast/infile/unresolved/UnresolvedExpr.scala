package verity.ast.infile.unresolved

import verity.ast._
import verity.ast.infile.{ResolvedOrUnresolvedExpr => RoUExpr, _}

import scala.collection.mutable.ArrayBuffer

sealed trait UnresolvedExpr extends HasText, HasType, RoUExpr

trait UnresolvedTypeExpr extends UnresolvedExpr {
  private[this] var _typ: Type = ToBeInferred(BuiltinTypes.objectType, NothingType, List.empty)
  override def typ: Type = _typ
  private[verity] def typ_=(typ: Type): Unit = this._typ = typ
}

class UnresolvedThisRef(override val textRange: TextRange) extends UnresolvedTypeExpr {
  override def text: String = "this"
}

class UnresolvedSuperRef(override val textRange: TextRange) extends UnresolvedTypeExpr {
  override def text: String = "super"
}

case class UnresolvedParenExpr(expr: RoUExpr, override val textRange: TextRange) extends UnresolvedExpr {
  def typ: Type = expr.typ
  override def text = s"(${expr.text})"
}

case class UnresolvedArraySelect(arr: RoUExpr, index: RoUExpr, bracketsTextRange: TextRange)
    extends UnresolvedTypeExpr {
  override def text: String = s"${arr.text}[${index.text}]"
  override def textRange: TextRange = TextRange(arr.textRange.start, bracketsTextRange.end)
}

case class UnresolvedBinaryExpr(left: RoUExpr, op: Op, right: RoUExpr) extends UnresolvedTypeExpr {
  override def text: String = s"(${left.text} ${op.text} ${right.text})"
  override def textRange: TextRange = TextRange(left.textRange.start, right.textRange.end)
}

case class UnresolvedUnaryPreExpr(op: Op, expr: RoUExpr) extends UnresolvedTypeExpr {
  override def text: String = s"(${op.text} ${expr.text})"
  override def textRange: TextRange = TextRange(op.textRange.start, expr.textRange.end)
}

case class UnresolvedUnaryPostExpr(expr: RoUExpr, op: Op) extends UnresolvedTypeExpr {
  override def text: String = s"(${expr.text}${op.text})"
  override def textRange: TextRange = TextRange(expr.textRange.start, op.textRange.end)
}

case class UnresolvedAssignmentExpr(lhs: RoUExpr, rhs: RoUExpr, extraOp: Option[Text])
    extends UnresolvedExpr {
  override def typ: Type = lhs.typ
  override def text: String = lhs.text + extraOp.fold("")(_.text) + "=" + rhs.text
  override def textRange: TextRange = TextRange(lhs.textRange.start, rhs.textRange.end)
}

class UnresolvedInstanceOf(
    val expr: RoUExpr,
    val typeRef: UnresolvedTypeRef,
    val instanceofTokRange: TextRange
) extends UnresolvedExpr {
  override val typ: Type = PrimitiveType.BooleanType
  override def text = s"${expr.text} instanceof ${typ.text}"
  override def textRange: TextRange = ???
}

case class UnresolvedFieldAccess(obj: RoUExpr, fieldName: Text) extends UnresolvedTypeExpr, HasText {
  override def textRange: TextRange = TextRange(obj.textRange.start, fieldName.textRange.end)
  override def text = s"${obj.text}.${fieldName.text}"
}

/** A method call whose caller and everything else have not been resolved.
  * @param objOrCls
  *   `None` if this method has no caller (present in same class or imported), otherwise it's a
  *   `Some` containing either a [[MultiDotRef]] or [[Expr]].
  */
case class UnresolvedMethodCall(
    objOrCls: Option[HasTextRange],
    methodName: Text,
    valArgs: UnresolvedArgList,
    typeArgs: Option[TypeArgList],
    givenArgs: Option[UnresolvedArgList], //TODO do givenArgs!!
    proofArgs: Option[UnresolvedArgList] //TODO do proofArgs!!
) extends UnresolvedTypeExpr {
  private[verity] var resolved: Option[Method] = None

  override def text: String = objOrCls.fold("")(_.text + ".")
    + typeArgs.fold("")(_.text)
    + methodName.text
    + valArgs.text
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)

  override def textRange = TextRange(
    objOrCls.orElse(typeArgs).getOrElse(methodName).textRange.start,
    proofArgs.orElse(givenArgs).getOrElse(valArgs).textRange.end
  )
}

case class UnresolvedArgList(
  args: List[RoUExpr],
  argsKind: ArgsKind,
  override val textRange: TextRange
) extends HasTextRange {
  override def text: String = args.view.map(_.text).mkString("(", ",", ")")
}

/** For something like `foo.bar.baz`, that does not come before a method call.
  */
case class MultiDotRefExpr(path: Seq[Text]) extends UnresolvedTypeExpr {
  override def text: String = path.view.map(_.text).mkString(".")
  override def textRange: TextRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}
