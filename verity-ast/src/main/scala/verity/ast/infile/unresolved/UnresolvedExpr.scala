package verity.ast.infile.unresolved

import verity.ast.*
import verity.ast.infile.{ResolvedOrUnresolvedExpr => RoUExpr, *}

import scala.collection.mutable.ListBuffer

sealed trait UnresolvedExpr extends HasText, HasType, RoUExpr

trait UnresolvedTypeExpr extends UnresolvedExpr {
  private[this] var _typ: Type = ToBeInferred(BuiltinTypes.objectType, NothingType, List.empty)
  override def typ: Type = _typ
  private[verity] def typ_=(typ: Type): Unit = this._typ = typ
}

/** An unresolved reference to a variable, package, or class
  */
//case class DotlessRef(refName: Text) extends Expr {
//  override def text: String = refName.text
//  override def textRange: TextRange = refName.textRange
//
//  def typ: Type = ???
//}

//case class DotRef(first: Expr, selected: Text)
//  extends Tree,
//    Expr,
//    HasText {
//  override def textRange: TextRange = TextRange(first.textRange.start, selected.textRange.end)
//  override def text = s"${first.text}.$selected"
//}

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
    val textRange: TextRange
) extends UnresolvedExpr {
  override val typ: Type = PrimitiveType.BooleanType
  override def text = s"${expr.text} instanceof ${typ.text}"
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
    objOrCls: Option[HasText],
    methodName: Text,
    valArgs: UnresolvedArgList,
    typeArgs: TypeArgList,
    givenArgs: Option[UnresolvedArgList], //TODO do givenArgs!!
    proofArgs: Option[UnresolvedArgList] //TODO do proofArgs!!
) extends UnresolvedTypeExpr {
  private[verity] var resolved: Option[Method] = None

  override def text: String = objOrCls.fold("")(_.toString)
    + typeArgs.text
    + methodName.text
    + valArgs.text
    + HasText.optText(givenArgs)
    + HasText.optText(proofArgs)
  override def textRange: TextRange = ???
}

case class UnresolvedArgList(args: List[RoUExpr], argsKind: ArgsKind, textRange: TextRange) extends HasText {
  override def text: String = args.view.map(_.text).mkString("(", ",", ")")
}

/** For something like `foo.bar.baz`, as long as it comes before a method call such as
  * `foo.bar.baz.blah(bleh)`.
  */
case class MultiDotRef(path: Seq[Text]) extends HasText {
  override def text: String = path.view.map(_.text).mkString(".")
  override def textRange: TextRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

/** For something like `foo.bar.baz`, that does not come before a method call.
  */
case class MultiDotRefExpr(path: Seq[Text]) extends UnresolvedTypeExpr {
  override def text: String = path.view.map(_.text).mkString(".")
  override def textRange: TextRange = TextRange(path.head.textRange.start, path.last.textRange.end)
}

case class UnresolvedTypeRef(
    path: Seq[Text],
    args: TypeArgList,
    private[this] var _resolved: Option[TypeDef] = None
) extends Type {
  override def fields: Iterable[Field] = resolved.fold(Nil)(_.fields)
  override def methods: Iterable[Method] = resolved.fold(Nil)(_.methods)

  override def superTypes: Iterable[Type] = resolved.fold(Nil)(_.superTypes)
  override def strictSubTypeOf(sup: Type): Boolean = ??? //resolved.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = ??? //resolved.fold(false){td => td.strictSuperTypeOf(sub)}

  override def text: String = HasText.seqText(path, ".") + args.text

  override def textRange: TextRange =
    if args.isEmpty || args.textRange.isSynthetic then
      TextRange(path.head.textRange.start, path.last.textRange.end)
    else TextRange(path.head.textRange.start, args.textRange.end)

  override def equals(other: Any): Boolean = other match {
    case tr: UnresolvedTypeRef =>
      resolved
        .flatMap(typ => tr.resolved.map(_ == typ))
        .getOrElse(false) && this.args.args.size == tr.args.args.size && this.args.args
        .lazyZip(tr.args.args)
        .forall(_ == _)
    case _ => false
  }

  def resolved: Option[TypeDef] = _resolved

  private[verity] def resolved_=(typeDef: TypeDef): Unit = _resolved = Some(typeDef)
}

case class UnresolvedWildcard(upper: Option[UnresolvedTypeRef], lower: Option[UnresolvedTypeRef])
    extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = lower.fold(false)(_.strictSuperTypeOf(sub))

  override def fields: Iterable[Field] = upper.fold(BuiltinTypes.objectType.fields)(_.fields)
  override def methods: Iterable[Method] = upper.fold(BuiltinTypes.objectType.methods)(_.methods)

  override def superTypes: Iterable[Type] = upper.fold(Nil)(_.superTypes)

  override def text: Nothing = ???
  override def textRange: TextRange = ???
}

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type): Boolean = lower.strictSuperTypeOf(sub)

  override def fields: Iterable[Field] = upper.fields
  override def methods: Iterable[Method] = upper.methods

  override def superTypes: Iterable[Type] = upper.superTypes.toSeq :+ upper

  override def text = "NOT INFERRED AAA!!!"
  override def textRange: TextRange = ???
}

/**
 * An unresolved expression with a semicolon after it
 */
class UnresolvedExprStmt(val expr: RoUExpr, val end: Int) extends Statement {
  override def text = s"${expr.text};"
  override def textRange = TextRange(expr.textRange.start, end)
}

