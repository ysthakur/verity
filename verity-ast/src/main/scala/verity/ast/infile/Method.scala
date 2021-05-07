package verity.ast.infile

import verity.ast._

import scala.collection.mutable.ListBuffer

trait Methodlike extends NamedTree {
  def returnType: Type
  def name: String
  def params: ParamList
  def typeParams: TypeParamList
  def givenParams: Option[ParamList]
  def proofParams: Option[ParamList]

  def body: Option[Block] //Option[Block|Expr]
  // def body_=(newBody: Option[Block] /*Option[Block|Expr]*/ ): Unit
}

trait Method extends Methodlike, ClassChild, HasText, HasModifiers {
  def isAbstract: Boolean = this.hasModifier(ModifierType.ABSTRACT)
  def isStatic: Boolean = this.hasModifier(ModifierType.STATIC)
  def isGiven: Boolean = this.hasModifier(ModifierType.GIVEN)
  def isProof: Boolean = this.hasModifier(ModifierType.PROOF)
}
object Method {
  /**
   * Merge a bunch of methods - if one is a subtype of another, keep the overridden method. Also return any methods
   * that can't be merged.
   * @return (mergedMethods, unmergeableMethods)
   */
  def mergeMethods(methods: Iterable[Method]): (Iterable[Method], Iterable[Iterable[Method]]) = {
    (methods, Nil) //TODO implement this!!!
  }
}

class NormMethod(
    val modifiers: ListBuffer[Modifier],
    val typeParams: TypeParamList,
    private[this] var _returnType: Type,
    val methodName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    val body: Option[Block],
    val textRange: TextRange
) extends Method {
  override def returnType: Type = _returnType
  private[verity] def returnType_=(typ: Type): Unit = _returnType = typ

  override def text = s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body
    .fold(";")(_.text)}"

  def name: String = methodName.text
}

class Constructor(
    val modifiers: ListBuffer[Modifier],
    val ctorName: Text,
    val params: ParamList,
    val givenParams: Option[ParamList],
    val proofParams: Option[ParamList],
    _body: Block,
    val textRange: TextRange,
    private[this] var _cls: () => HasCtors
) extends Method {
  lazy val cls: HasCtors = _cls()
  override lazy val typeParams: TypeParamList = TypeParamList(cls.typeParams.params, TextRange.synthetic)
  override lazy val returnType: Type = cls.makeRef

  val body: Option[Block] = Some(_body)

  override def text =
    s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"

  def name: String = ctorName.text
}

object Constructor {
  def defaultCtor(cls: HasCtors): Constructor =
    Constructor(
        cls.accessModifier.map(Modifier(_, TextRange.synthetic)).to(ListBuffer),
        Text(cls.name),
        Empty[ParamList],
        None,
        None,
        Block.empty(VoidTypeRef(TextRange.synthetic)),
        Empty[TextRange],
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
    isErased: Boolean,
    override val textRange: TextRange
) extends VariableDecl,
      HasText,
      HasAnnotations,
      NamedTree {
  def modifiers: Iterable[Modifier] = ???
//    if (isGiven) if (isErased) List(Modifier(ModifierType.GIVEN))
  def initExpr: None.type = None
  override def text: String =
    (if (isGiven) "given " else "") +
      (if (isErased) "erased " else "") +
      s"${annotations.map(_.text).mkString(" ")} ${typ.text} $name"

  def name: String = paramName.text
}

case class ParamList(params: List[Parameter], textRange: TextRange) extends Tree, HasText {
  override def text: String = HasText.seqText(params, ",", "(", ")")
}

given Empty[ParamList] with
  def empty: ParamList = ParamList(Nil, TextRange.synthetic)
