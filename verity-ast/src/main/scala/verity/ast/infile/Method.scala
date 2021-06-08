package verity.ast.infile

import verity.ast._

import scala.collection.mutable.ArrayBuffer

trait Methodlike extends NamedTree {
  def returnType: Type
  /**
   * The proofs returned by this method/function
   */
  def proofs: Iterable[Type]
  def name: String
  var params: ParamList
  def typeParams: TypeParamList
  var givenParams: Option[ParamList]
  var proofParams: Option[ParamList]

  def nameRange: TextRange
  def body: Option[Block] //Option[Block|Expr]

  def isCtor: Boolean =
    this.isInstanceOf[Constructor] || this.isInstanceOf[unresolved.UnresolvedConstructor]
  // def body_=(newBody: Option[Block] /*Option[Block|Expr]*/ ): Unit
}

trait Method extends Methodlike, ClassChild, HasText, HasModifiers {
  private[verity] var _proofs: Iterable[Type]
  override def proofs = _proofs
}
object Method {

  /** Merge a bunch of methods - if one is a subtype of another, keep the overridden method. Also return any methods
    * that can't be merged.
    * @return (mergedMethods, unmergeableMethods)
    */
  def mergeMethods(methods: Iterable[Method]): (Iterable[Method], Iterable[Iterable[Method]]) = {
    (methods, Nil) //TODO implement this!!!
  }
}

//TODO look at thrown exceptions
class NormMethod(
  val modifiers: ArrayBuffer[Modifier],
  val typeParams: TypeParamList,
  private[verity] var _returnType: Type,
  private[verity] var _proofs: Iterable[Type],
  val methodName: Text,
  var params: ParamList,
  var givenParams: Option[ParamList],
  var proofParams: Option[ParamList],
  val thrownExceptions: Iterable[Type],
  val body: Option[Block]
) extends Method {
  override def text =
    s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body.fold(";")(_.text)}"

  override def returnType: Type = _returnType
  override def proofs: Iterable[Type] = _proofs

  private[verity] def returnType_=(typ: Type): Unit = _returnType = typ

  def name: String = methodName.text
  override def nameRange = methodName.textRange
}

class Constructor(
  val modifiers: ArrayBuffer[Modifier],
  val name: String,
  val nameRange: TextRange,
  var params: ParamList,
  var givenParams: Option[ParamList],
  var proofParams: Option[ParamList],
  val thrownExceptions: Iterable[Type],
  _body: Block,
  private[this] var _cls: () => HasCtors
) extends Method {
  lazy val cls: HasCtors = _cls()
  override lazy val typeParams: TypeParamList =
    TypeParamList(cls.typeParams.params, TextRange.synthetic)
  override lazy val returnType: Type = cls.makeRef
  private[verity] var _proofs: Iterable[Type] = Nil

  override def proofs = Nil

  val body: Option[Block] = Some(_body)

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"
}

object Constructor {
  def defaultCtor(cls: HasCtors): Constructor =
    Constructor(
      cls.accessModifier.map(Modifier(_, TextRange.synthetic)).to(ArrayBuffer),
      cls.name,
      TextRange.synthetic,
      Empty[ParamList],
      None,
      None,
      Nil,
      Block.empty(VoidTypeRef(TextRange.synthetic)),
      () => cls
    )
}

case class MethodGroup(name: String, methods: Iterable[Method]) extends ClassChild {
  def merge(other: MethodGroup): MethodGroup = MethodGroup(name, this.methods ++ other.methods)
}

case class Parameter(
  annotations: List[Annotation],
  typ: Type,
  paramName: Text,
  override val isGiven: Boolean,
  override val isProof: Boolean
) extends VariableDecl,
      HasText,
      HasAnnotations,
      NamedTree {
  def modifiers: Iterable[Modifier] =
    if (isProof) List(Modifier(ModifierType.PROOF, TextRange.synthetic))
    else if (isGiven) List(Modifier(ModifierType.GIVEN, TextRange.synthetic))
    else Nil
  def initExpr: None.type = None
  override def text: String =
    s"${annotations.map(_.text).mkString(" ")} ${typ.text} $name"

  def name: String = paramName.text
}

case class ParamList(params: List[Parameter], override val textRange: TextRange, kind: ParamListKind = ParamListKind.NORMAL)
    extends Tree,
      HasTextRange {
  override def text: String = HasText.seqText(params, ",", "(", ")")
}

enum ParamListKind {
  case NORMAL, GIVEN, PROOF
}

given Empty[ParamList] with
  def empty: ParamList = ParamList(Nil, TextRange.synthetic)
