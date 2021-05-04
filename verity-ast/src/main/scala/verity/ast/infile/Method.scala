package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

trait Methodlike extends NamedTree {
  def returnType: Type
  def name: String
  def params: ParamList
  def typeParams: TypeParamList

  def body: Option[Block] //Option[Block|Expr]
  // def body_=(newBody: Option[Block] /*Option[Block|Expr]*/ ): Unit
}

trait Method extends Methodlike, ClassChild, HasText, HasModifiers {
  def isAbstract: Boolean = this.hasModifier(ModifierType.ABSTRACT)
  def isGiven: Boolean = this.hasModifier(ModifierType.GIVEN)
  def isProof: Boolean = this.hasModifier(ModifierType.PROOF)
}

class NormMethod(
    val modifiers: ListBuffer[Modifier],
    val typeParams: TypeParamList,
    val returnType: Type,
    val name: String,
    val params: ParamList,
    val body: Option[Block],
    val textRange: TextRange
) extends Method {
  def text = s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body
    .fold(";")(_.text)}"
}

class Constructor(
    val modifiers: ListBuffer[Modifier],
    val name: String,
    val params: ParamList,
    _body: Block,
    val textRange: TextRange,
    _cls: => HasCtors
) extends Method {
  lazy val cls = _cls //TODO find a better way to do this

  lazy val typeParams = TypeParamList(cls.typeParams.params, TextRange.synthetic)
  lazy val returnType = TypeRef(cls.name, typeParams.params.view.map(_.makeRef()), TextRange.synthetic)

  val body = Some(_body)

  def text = s"${modifiers.map(_.text).mkString(" ")} $name ${params.text} ${body.fold(";")(_.text)}"
}

object Constructor {
  def defaultCtor(cls: HasCtors): Constructor =
    Constructor(
      cls.accessModifier.map(Modifier(_, TextRange.synthetic)).to(ListBuffer),
      cls.name,
      Empty[ParamList],
      Empty[Block],
      Empty[TextRange],
      cls
    )
}

case class MethodGroup(name: String, methods: Iterable[Method]) extends ClassChild {
  def merge(other: MethodGroup) = MethodGroup(name, this.methods ++ other.methods)
}

case class Parameter(
    annotations: List[Annotation],
    paramType: Type,
    name: String,
    isGiven: Boolean,
    isErased: Boolean,
    override val textRange: TextRange
) extends HasText,
      HasAnnotations,
      NamedTree {
  def text: String =
    (if (isGiven) "given " else "") +
      (if (isErased) "erased " else "") +
      s"${annotations.map(_.text).mkString(" ")} ${paramType.text} $name"
}

case class ParamList(params: List[Parameter], val textRange: TextRange) extends Tree, HasText {
  def text: String = HasText.seqText(params, ",", "(", ")") 
}

given Empty[ParamList] with
  def empty = ParamList(Nil, TextRange.synthetic)
