package verity.ast.infile

import verity.ast.*

import scala.collection.mutable.ListBuffer

trait Methodlike {
  def returnType: Type
  def name: String
  def params: ParamList
  
  def body: Option[Block] //Option[Block|Expr]
  def body_=(newBody: Option[Block] /*Option[Block|Expr]*/): Unit
}

class Method(
    override val modifiers: ListBuffer[Modifier],
    val returnType: Type,
    val name: String,
    val params: ParamList,
    private var _body: Option[Block],
    val isCtor: Boolean,
    val textRange: TextRange
) extends Methodlike, NamedTree, HasText, HasModifiers {
  def text = s"${modifiers.map(_.text).mkString(" ")} ${returnType.text} $name ${params.text} ${body.fold(";")(_.text)}"
  override def body: Option[Block] = _body
  def body_=(newBody: Option[Block]): Unit = _body = newBody
}

case class MethodGroup(name: String, methods: Iterable[Method]) extends NamedTree {
  def merge(other: MethodGroup) = MethodGroup(name, this.methods ++ other.methods)
}

case class Parameter(
  annotations: List[Annotation],
  paramType: Type,
  name: String,
  isGiven: Boolean,
  isErased: Boolean,
  override val textRange: TextRange
) extends HasText, HasAnnotations, NamedTree {
  def text: String =
    (if (isGiven) "given " else "") +
    (if (isErased) "erased " else "") +
    s"${annotations.map(_.text).mkString(" ")} ${paramType.text} $name"
}

case class ParamList(params: List[Parameter], val textRange: TextRange) extends Tree, HasText {
  override def text: String = params.map(_.text).mkString("(", ",", ")")
}