package verity.ast.infile.unresolved

import verity.ast._
import verity.ast.infile.{ResolvedOrUnresolvedExpr => RoUExpr, _}

import scala.collection.mutable.ArrayBuffer

case class UnresolvedTypeRef(
  path: Seq[Text],
  args: TypeArgList,
  private[this] var _resolved: Option[TypeDef] = None
) extends Type, ResolvedOrUnresolvedTypeRef, HasText, HasTextRange {
  override def fields: Iterable[Field] = resolved.fold(Nil)(_.fields)

  def resolved: Option[TypeDef] = _resolved

  private[verity] def resolved_=(typeDef: TypeDef): Unit = _resolved = Some(typeDef)

  override def methods: Iterable[Method] = resolved.fold(Nil)(_.methods)

  override def superTypes: Iterable[Type] = resolved.fold(Nil)(_.superTypes)
  override def strictSubTypeOf(sup: Type): Boolean = false
  override def strictSuperTypeOf(sub: Type): Boolean = false

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
}

case class UnresolvedWildcard(upper: Option[Type], lower: Option[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.fold(false)(_.strictSubTypeOf(sup))
  override def strictSuperTypeOf(sub: Type): Boolean = lower.fold(false)(_.strictSuperTypeOf(sub))

  override def fields: Iterable[Field] = upper.fold(BuiltinTypes.objectTypeDef.makeRef.fields)(_.fields)
  override def methods: Iterable[Method] = upper.fold(BuiltinTypes.objectTypeDef.makeRef.methods)(_.methods)

  override def superTypes: Iterable[Type] = upper.fold(Nil)(_.superTypes)

  override def text = s"? extends $upper super $lower"
  // override def textRange = ???
}

case class ToBeInferred(upper: Type, lower: Type, not: List[Type]) extends Type {
  override def strictSubTypeOf(sup: Type): Boolean = upper.strictSubTypeOf(sup)
  override def strictSuperTypeOf(sub: Type): Boolean = lower.strictSuperTypeOf(sub)

  override def fields: Iterable[Field] = upper.fields
  override def methods: Iterable[Method] = upper.methods

  override def superTypes: Iterable[Type] = upper.superTypes.toSeq :+ upper

  override def text = "NOT INFERRED AAA!!!"
  // override def textRange = ???
}
